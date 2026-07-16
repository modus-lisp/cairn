;;;; status.lisp — what changed: HEAD vs index vs working tree.
;;;;
;;;; git's three-way view of a repo.  The index sits between the last commit
;;;; (HEAD) and the files on disk: differences index-vs-HEAD are "staged",
;;;; differences worktree-vs-index are "unstaged", and files on disk that the
;;;; index has never heard of are "untracked".  We flatten HEAD's tree to a
;;;; path→SHA map, read the index, and walk the working tree, then diff the
;;;; three.

(in-package #:cairn)

(defun flatten-tree (repo tree-sha &optional (prefix ""))
  "Alist of (repo-relative-path . (mode . sha)) for every blob under TREE-SHA."
  (let ((out '()))
    (dolist (e (parse-tree (object-data repo tree-sha)))
      (let ((path (if (string= prefix "") (tree-entry-name e)
                      (concatenate 'string prefix "/" (tree-entry-name e)))))
        (if (string= (tree-entry-mode e) "40000")
            (setf out (nconc out (flatten-tree repo (tree-entry-sha e) path)))
            (push (cons path (cons (tree-entry-mode e) (tree-entry-sha e))) out))))
    out))

(defun head-tree-map (repo)
  "Path→sha hash-table for the tree of HEAD (empty on an unborn branch)."
  (let ((map (make-hash-table :test 'equal))
        (head (ignore-errors (head-commit repo))))
    (when head
      (dolist (e (flatten-tree repo (commit-tree (parse-commit (object-data repo head)))))
        (setf (gethash (car e) map) (cddr e))))
    map))

(defun hash-blob-file (repo rel)
  "The blob SHA the working-tree file (or symlink) at repo-relative REL hashes to."
  (if (eq (wts-type (wt-lstat repo rel)) :symlink)
      (hash-object :blob (string->bytes (wt-read-symlink repo rel)))
      (hash-object :blob (wt-read-file repo rel))))

(defun worktree-modified-p (repo rel entry)
  "Does the worktree file at REL differ from its index ENTRY?  Fast path on
   size+mtime, else compare the content hash."
  (let ((st (wt-lstat repo rel)))
    (if (and (= (logand (wts-size st) #xffffffff) (logand (ie-size entry) #xffffffff))
             (= (logand (wts-mtime st) #xffffffff) (logand (ie-mtime entry) #xffffffff)))
        nil
        (not (string= (hash-blob-file repo rel) (ie-sha entry))))))

(defun walk-worktree (repo)
  "Repo-relative paths of every file in the working tree, excluding .git."
  (wt-walk repo ""))

(defun status (repo)
  "Return (values STAGED UNSTAGED UNTRACKED UNMERGED).  STAGED/UNSTAGED are
   alists of (path . :added/:modified/:deleted); UNTRACKED and UNMERGED are lists
   of paths (UNMERGED = paths with conflict stages recorded by a merge)."
  (with-oid (repo)
  (let* ((index (read-index repo))
         (index-map (make-hash-table :test 'equal))
         (head (head-tree-map repo))
         (unmerged '())
         (staged '()) (unstaged '()) (untracked '()))
    (loop for e across index do
      (if (plusp (ie-stage e))
          (pushnew (ie-path e) unmerged :test #'string=)
          (setf (gethash (ie-path e) index-map) e)))
    ;; staged: index vs HEAD (stage-0 entries only)
    (maphash (lambda (path e)
               (let ((h (gethash path head)))
                 (cond ((null h) (push (cons path :added) staged))
                       ((not (string= h (ie-sha e))) (push (cons path :modified) staged)))))
             index-map)
    (maphash (lambda (path sha) (declare (ignore sha))
               (unless (or (gethash path index-map) (member path unmerged :test #'string=))
                 (push (cons path :deleted) staged)))
             head)
    ;; unstaged: worktree vs index
    (maphash (lambda (path e)
               (cond ((not (wt-exists-p repo path)) (push (cons path :deleted) unstaged))
                     ((worktree-modified-p repo path e) (push (cons path :modified) unstaged))))
             index-map)
    ;; untracked
    (dolist (path (walk-worktree repo))
      (unless (or (gethash path index-map) (member path unmerged :test #'string=))
        (push path untracked)))
    (values (sort staged #'string< :key #'car)
            (sort unstaged #'string< :key #'car)
            (sort untracked #'string<)
            (sort unmerged #'string<)))))

(defun print-status (repo &optional (stream *standard-output*))
  "A git-style status summary."
  (multiple-value-bind (staged unstaged untracked unmerged) (status repo)
    (multiple-value-bind (kind ref) (head-ref repo)
      (format stream "On ~a~%" (if (eq kind :symbolic)
                                   (format nil "branch ~a" (subseq ref (1+ (or (position #\/ ref :from-end t) -1))))
                                   (format nil "detached HEAD ~a" (subseq ref 0 8)))))
    (flet ((section (title rows fmt)
             (when rows
               (format stream "~%~a:~%" title)
               (dolist (r rows) (funcall fmt r)))))
      (section "Unmerged paths" unmerged
               (lambda (p) (format stream "  both modified: ~a~%" p)))
      (section "Changes to be committed" staged
               (lambda (r) (format stream "  ~9a ~a~%" (string-downcase (cdr r)) (car r))))
      (section "Changes not staged for commit" unstaged
               (lambda (r) (format stream "  ~9a ~a~%" (string-downcase (cdr r)) (car r))))
      (section "Untracked files" untracked
               (lambda (p) (format stream "  ~a~%" p)))
      (when (and (null staged) (null unstaged) (null untracked) (null unmerged))
        (format stream "~%nothing to commit, working tree clean~%")))))
