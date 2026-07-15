;;;; repository.lisp — the repository handle and object lookup.

(in-package #:cairn)

(defstruct (repository (:conc-name repo-)) path git-dir)

(defun open-repository (path)
  "Open the git repository at PATH (which must contain a .git directory, or be a
   bare repository)."
  (let* ((base (uiop:ensure-directory-pathname path))
         (dotgit (merge-pathnames ".git/" base)))
    (cond ((probe-file (merge-pathnames "HEAD" dotgit))
           (make-repository :path base :git-dir dotgit))
          ((probe-file (merge-pathnames "HEAD" base))       ; bare repo
           (make-repository :path base :git-dir base))
          (t (error "cairn: not a git repository: ~a" path)))))

(defmacro with-repository ((var path) &body body)
  `(let ((,var (open-repository ,path))) ,@body))

(defun object-path (repo sha)
  (merge-pathnames (format nil "objects/~a/~a" (subseq sha 0 2) (subseq sha 2))
                   (repo-git-dir repo)))

(defun read-object (repo sha)
  "Return (values TYPE-KEYWORD CONTENT-BYTES) for the object named by the 40-hex
   SHA.  Looks in the loose object store, then packfiles."
  (let ((path (object-path repo sha)))
    (if (probe-file path)
        (parse-object (zlib-decompress (slurp-bytes path)))
        (read-pack-object repo sha))))

(defun object-type (repo sha) (values (read-object repo sha)))
(defun object-data (repo sha) (nth-value 1 (read-object repo sha)))

;;; packfiles arrive in the next phase; until then loose objects only.
(defun read-pack-object (repo sha)
  (declare (ignore repo))
  (error "cairn: object ~a not found as a loose object (packfiles not yet supported)" sha))
