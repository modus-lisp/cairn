;;;; checkout.lisp — materialise a commit's tree into the working directory.
;;;;
;;;; Reading objects gets us the *content* of a commit; checkout puts it on
;;;; disk.  Two halves: walk the commit's tree writing every blob as a file
;;;; (with its mode — regular, executable, or symlink) and recursing into
;;;; subtrees; then write the .git/index (the "dircache"), git's binary record
;;;; of what the working tree is supposed to be.  With a faithful index — each
;;;; entry carrying the file's lstat metadata and blob SHA — `git status` on
;;;; the result reports a clean tree, no re-hashing needed.

(in-package #:cairn)

(defun %push-be16 (vec u)
  (vector-push-extend (logand (ash u -8) #xff) vec)
  (vector-push-extend (logand u #xff) vec))

;;; git index modes: 4-bit object type + 9-bit unix perms.
(defun index-mode (tree-mode)
  (cond ((string= tree-mode "100644") #o100644)
        ((string= tree-mode "100755") #o100755)
        ((string= tree-mode "120000") #o120000)         ; symlink
        ((string= tree-mode "160000") #o160000)         ; gitlink (submodule)
        (t (error "cairn: unexpected tree entry mode ~a" tree-mode))))

(defun write-worktree (repo tree-sha dir prefix entries)
  "Recursively write the tree named TREE-SHA into directory DIR.  PREFIX is the
   repo-relative path so far; ENTRIES is an adjustable vector collecting index
   rows as (path . lstat-plist)."
  (multiple-value-bind (type content) (read-object repo tree-sha)
    (declare (ignore type))
    (dolist (e (parse-tree content))
      (let* ((mode (tree-entry-mode e))
             (name (tree-entry-name e))
             (relpath (if (string= prefix "") name (concatenate 'string prefix "/" name)))
             (path (merge-pathnames name dir)))
        (cond
          ((string= mode "40000")                        ; subtree
           (let ((sub (uiop:ensure-directory-pathname path)))
             (ensure-directories-exist sub)
             (write-worktree repo (tree-entry-sha e) sub relpath entries)))
          ((string= mode "160000"))                      ; submodule: no gitlink checkout
          ((string= mode "120000")                       ; symlink
           (multiple-value-bind (bt target) (read-object repo (tree-entry-sha e))
             (declare (ignore bt))
             (uiop:delete-file-if-exists path)
             (sb-posix:symlink (ascii target) (namestring path))
             (vector-push-extend (cons relpath (index-row path (tree-entry-sha e) mode))
                                 entries)))
          (t                                             ; regular / executable file
           (multiple-value-bind (bt blob) (read-object repo (tree-entry-sha e))
             (declare (ignore bt))
             (write-bytes path blob)
             (when (string= mode "100755") (sb-posix:chmod (namestring path) #o755))
             (vector-push-extend (cons relpath (index-row path (tree-entry-sha e) mode))
                                 entries))))))))

(defun index-row (path sha tree-mode)
  "Gather the index fields for the file at PATH (already written)."
  (let ((st (sb-posix:lstat (namestring path))))
    (list :ctime (sb-posix:stat-ctime st) :mtime (sb-posix:stat-mtime st)
          :dev (sb-posix:stat-dev st) :ino (sb-posix:stat-ino st)
          :mode (index-mode tree-mode) :uid (sb-posix:stat-uid st)
          :gid (sb-posix:stat-gid st) :size (sb-posix:stat-size st)
          :sha (hex->bytes sha))))

(defun write-index (git-dir entries)
  "Write ENTRIES (a vector of (path . plist)) as a v2 .git/index."
  (let ((sorted (sort (coerce entries 'list) #'string< :key #'car))
        (buf (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (map nil (lambda (b) (vector-push-extend b buf)) (string->bytes "DIRC"))
    (%push-be32 buf 2)
    (%push-be32 buf (length sorted))
    (loop for (path . f) in sorted do
      (let* ((name (string->bytes path))
             (start (fill-pointer buf)))
        (%push-be32 buf (logand (getf f :ctime) #xffffffff)) (%push-be32 buf 0)
        (%push-be32 buf (logand (getf f :mtime) #xffffffff)) (%push-be32 buf 0)
        (%push-be32 buf (logand (getf f :dev) #xffffffff))
        (%push-be32 buf (logand (getf f :ino) #xffffffff))
        (%push-be32 buf (getf f :mode))
        (%push-be32 buf (getf f :uid))
        (%push-be32 buf (getf f :gid))
        (%push-be32 buf (logand (getf f :size) #xffffffff))
        (map nil (lambda (b) (vector-push-extend b buf)) (getf f :sha))
        (%push-be16 buf (min (length name) #xfff))
        (map nil (lambda (b) (vector-push-extend b buf)) name)
        ;; pad with NULs so the entry length (from ctime) is a multiple of 8
        (let* ((len (- (fill-pointer buf) start))
               (pad (- (logand (+ len 8) (lognot 7)) len)))
          (dotimes (_ pad) (vector-push-extend 0 buf)))))
    (let ((digest (sha1 (subseq buf 0 (fill-pointer buf)))))
      (map nil (lambda (b) (vector-push-extend b buf)) digest))
    (write-bytes (merge-pathnames "index" git-dir)
                 (coerce buf '(simple-array (unsigned-byte 8) (*))))))

(defun checkout (repo &optional (commit (head-commit repo)))
  "Materialise COMMIT (default HEAD) into REPO's working directory and write the
   matching .git/index.  Returns the number of files written."
  (let* ((tree (commit-tree (parse-commit (object-data repo commit))))
         (entries (make-array 64 :adjustable t :fill-pointer 0)))
    (write-worktree repo tree (repo-path repo) "" entries)
    (write-index (repo-git-dir repo) entries)
    (length entries)))
