;;;; commit.lisp — the write-side porcelain: add, write-tree, commit.
;;;;
;;;; This is where cairn stops being read-only.  `add` hashes a working-tree
;;;; file into a blob and stages it in the index; `write-tree` turns the flat
;;;; index into git's nested tree objects; `commit` writes a commit object that
;;;; points at that tree and the current HEAD, then advances the branch ref.
;;;; The result is an ordinary commit — real git logs it, fscks it, builds on it.

(in-package #:cairn)

;;; ---- HEAD / refs ------------------------------------------------------------

(defun head-ref (repo)
  "Where HEAD points: (values :symbolic \"refs/heads/…\") or (values :detached SHA)."
  (let ((head (string-trim '(#\Newline #\Space #\Return)
                           (slurp-string (merge-pathnames "HEAD" (repo-git-dir repo))))))
    (if (and (> (length head) 5) (string= (subseq head 0 5) "ref: "))
        (values :symbolic (subseq head 5))
        (values :detached head))))

(defun update-ref (repo refname sha)
  "Point REFNAME (e.g. \"refs/heads/master\") at SHA."
  (write-text-file (merge-pathnames refname (repo-git-dir repo)) (format nil "~a~%" sha)))

;;; ---- add --------------------------------------------------------------------

(defun add (repo &rest paths)
  "Stage the working-tree files at repo-relative PATHS: write each as a blob and
   upsert its index entry.  Writes the index and returns the staged paths."
  (with-oid (repo)
  (let* ((git-dir (repo-git-dir repo))
         (index (coerce (read-index git-dir) 'list)))
    (dolist (rel paths)
      (let ((abs (merge-pathnames rel (repo-path repo))))
        (multiple-value-bind (sha mode) (write-blob-from-file repo abs)
          (setf index (cons (stat-index-entry abs rel sha mode)
                            (remove rel index :key #'ie-path :test #'string=))))))
    (write-index git-dir index)
    paths)))

;;; ---- write-tree -------------------------------------------------------------

(defun %tree-sort-key (entry)
  "git orders tree entries by name, but a subtree sorts as if it ended in '/'."
  (destructuring-bind (mode name sha) entry
    (declare (ignore sha))
    (if (string= mode "40000") (concatenate 'string name "/") name)))

(defun build-tree (repo entries prefix)
  "Write the tree for the index ENTRIES under PREFIX; return its SHA."
  (let ((subdirs (make-hash-table :test 'equal))
        (rows '()))
    (dolist (e entries)
      (let* ((rel (subseq (ie-path e) (length prefix)))
             (slash (position #\/ rel)))
        (if slash
            (push e (gethash (subseq rel 0 slash) subdirs))
            (push (list (index-mode->tree-mode (ie-mode e)) rel (hex->bytes (ie-sha e))) rows))))
    (maphash (lambda (dir es)
               (push (list "40000" dir
                           (hex->bytes (build-tree repo (nreverse es)
                                                   (concatenate 'string prefix dir "/"))))
                     rows))
             subdirs)
    (let ((buf (byte-buffer)))
      (dolist (row (sort rows #'string< :key #'%tree-sort-key))
        (destructuring-bind (mode name sha) row
          (push-bytes buf (string->bytes (format nil "~a ~a" mode name)))
          (vector-push-extend 0 buf)
          (push-bytes buf sha)))
      (write-object repo :tree (coerce buf 'u8v)))))

(defun write-tree (repo)
  "Write tree objects for the current index; return the root tree SHA."
  (with-oid (repo)
  (build-tree repo (coerce (read-index (repo-git-dir repo)) 'list) "")))

;;; ---- commit -----------------------------------------------------------------

(defun unix-now () (- (get-universal-time) 2208988800))

(defun commit (repo &key message
                         (author "cairn <cairn@localhost>")
                         (committer author)
                         (time (unix-now))
                         (timezone "+0000"))
  "Create a commit of the current index whose parent is the current HEAD, write
   it, and advance the branch (or detached HEAD).  Returns the new commit SHA."
  (with-oid (repo)
  (unless message (error "cairn: commit requires a :message"))
  (let* ((tree (write-tree repo))
         (parent (ignore-errors (head-commit repo)))    ; NIL for an unborn branch
         (msg (if (and (plusp (length message))
                       (char= (char message (1- (length message))) #\Newline))
                  message (concatenate 'string message (string #\Newline))))
         (text (with-output-to-string (s)
                 (format s "tree ~a~%" tree)
                 (when parent (format s "parent ~a~%" parent))
                 (format s "author ~a ~d ~a~%" author time timezone)
                 (format s "committer ~a ~d ~a~%" committer time timezone)
                 (format s "~%~a" msg)))
         (sha (write-object repo :commit (sb-ext:string-to-octets text :external-format :utf-8))))
    (multiple-value-bind (kind ref) (head-ref repo)
      (ecase kind
        (:symbolic (update-ref repo ref sha))
        (:detached (write-text-file (merge-pathnames "HEAD" (repo-git-dir repo))
                                    (format nil "~a~%" sha)))))
    sha)))
