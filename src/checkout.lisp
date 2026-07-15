;;;; checkout.lisp — materialise a commit's tree into the working directory.
;;;;
;;;; Reading objects gets us the *content* of a commit; checkout puts it on
;;;; disk.  Walk the commit's tree writing every blob as a file (with its mode —
;;;; regular, executable, or symlink) and recursing into subtrees; then write
;;;; the .git/index (index.lisp) so `git status` on the result reports a clean
;;;; tree, no re-hashing needed.

(in-package #:cairn)

(defun write-worktree (repo tree-sha dir prefix entries)
  "Recursively write the tree named TREE-SHA into directory DIR.  PREFIX is the
   repo-relative path so far; ENTRIES is an adjustable vector collecting the
   INDEX-ENTRY rows for each file written."
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
             (vector-push-extend (stat-index-entry path relpath (tree-entry-sha e) mode) entries)))
          (t                                             ; regular / executable file
           (multiple-value-bind (bt blob) (read-object repo (tree-entry-sha e))
             (declare (ignore bt))
             (write-bytes path blob)
             (when (string= mode "100755") (sb-posix:chmod (namestring path) #o755))
             (vector-push-extend (stat-index-entry path relpath (tree-entry-sha e) mode) entries))))))))

(defun checkout (repo &optional (commit (head-commit repo)))
  "Materialise COMMIT (default HEAD) into REPO's working directory and write the
   matching .git/index.  Files tracked by the old index but absent from COMMIT
   are removed, so a fast-forward leaves a clean tree.  Returns the file count."
  (let* ((tree (commit-tree (parse-commit (object-data repo commit))))
         (old (read-index (repo-git-dir repo)))
         (entries (make-array 64 :adjustable t :fill-pointer 0)))
    (write-worktree repo tree (repo-path repo) "" entries)
    (let ((kept (make-hash-table :test 'equal)))
      (loop for e across entries do (setf (gethash (ie-path e) kept) t))
      (loop for e across old
            unless (gethash (ie-path e) kept)
              do (uiop:delete-file-if-exists (merge-pathnames (ie-path e) (repo-path repo)))))
    (write-index (repo-git-dir repo) entries)
    (length entries)))
