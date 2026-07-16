;;;; fs-backend.lisp — the storage seam: where cairn's bytes actually live.
;;;;
;;;; cairn's git store (objects, refs, index, packs, config, HEAD) is a set of
;;;; named byte blobs under the git directory.  Rather than call the host
;;;; filesystem directly, the store goes through a small backend protocol —
;;;; ~9 ops on git-dir-relative string paths ("/"-separated) — so the SAME cairn
;;;; can keep a repository on the host filesystem (the default) or inside a
;;;; cabinet filesystem (which lives in a single pagetree file), which is what
;;;; lets cairn run where there is no host filesystem at all (modus).
;;;;
;;;; A backend is a vtable of closures, so cairn's core needs no dependency on
;;;; any particular backend; the cabinet backend is an opt-in add-on system
;;;; (cairn/cabinet) that supplies its own closures.

(in-package #:cairn)

(defstruct (fs-backend (:conc-name fsb-))
  read-bytes            ; (relpath)        -> (u8v | nil)      whole-file read
  read-string           ; (relpath)        -> (string | nil)  whole-file read, utf-8
  write-bytes           ; (relpath bytes)  -> t                create/replace, mkdir -p
  write-string          ; (relpath text)   -> t
  exists-p              ; (relpath)        -> boolean
  delete-file           ; (relpath)        -> t                absent is fine
  dir-exists-p          ; (reldir)         -> boolean
  list-files            ; (reldir)         -> (basename …)     files directly in reldir
  walk-files)           ; (reldir)         -> (git-relpath …)  files anywhere under reldir

;;; ---- the wrappers cairn's store code calls ---------------------------------

(declaim (inline repo-fsb))
(defun repo-fsb (repo) (repo-backend repo))

(defun fs-read-bytes  (repo rel)        (funcall (fsb-read-bytes  (repo-fsb repo)) rel))
(defun fs-read-string (repo rel)        (funcall (fsb-read-string (repo-fsb repo)) rel))
(defun fs-write-bytes (repo rel bytes)  (funcall (fsb-write-bytes (repo-fsb repo)) rel bytes))
(defun fs-write-string (repo rel text)  (funcall (fsb-write-string (repo-fsb repo)) rel text))
(defun fs-exists-p    (repo rel)        (funcall (fsb-exists-p    (repo-fsb repo)) rel))
(defun fs-delete-file (repo rel)        (funcall (fsb-delete-file (repo-fsb repo)) rel))
(defun fs-dir-exists-p (repo rel)       (funcall (fsb-dir-exists-p (repo-fsb repo)) rel))
(defun fs-list-files  (repo rel)        (funcall (fsb-list-files  (repo-fsb repo)) rel))
(defun fs-walk-files  (repo rel)        (funcall (fsb-walk-files  (repo-fsb repo)) rel))

(defun loose-object-relpath (sha)
  "The git-dir-relative path of the loose object named SHA (hex)."
  (format nil "objects/~a/~a" (subseq sha 0 2) (subseq sha 2)))

;;; ---- the host filesystem backend (the default) -----------------------------

(defun host-walk-files (dir git-dir)
  "Git-dir-relative paths of every file anywhere under DIR (a pathname)."
  (let ((out '()))
    (labels ((walk (d)
               (dolist (p (uiop:directory-files d)) (push (enough-namestring p git-dir) out))
               (dolist (s (uiop:subdirectories d)) (walk s))))
      (when (uiop:directory-exists-p dir) (walk dir)))
    (nreverse out)))

(defun make-host-backend (git-dir)
  "A backend that reads and writes the real host filesystem under GIT-DIR (a
   directory pathname).  This is byte-for-byte the behavior cairn had before the
   seam existed."
  (flet ((full (rel) (merge-pathnames rel git-dir)))
    (make-fs-backend
     :read-bytes   (lambda (rel) (let ((p (full rel))) (when (probe-file p) (slurp-bytes p))))
     :read-string  (lambda (rel) (slurp-string (full rel)))
     :write-bytes  (lambda (rel bytes) (write-bytes (full rel) bytes) t)
     :write-string (lambda (rel text) (write-text-file (full rel) text) t)
     :exists-p     (lambda (rel) (and (probe-file (full rel)) t))
     :delete-file  (lambda (rel) (uiop:delete-file-if-exists (full rel)) t)
     :dir-exists-p (lambda (rel) (and (uiop:directory-exists-p (full rel)) t))
     :list-files   (lambda (rel)
                     (let ((d (full rel)))
                       (when (uiop:directory-exists-p d)
                         (mapcar #'file-namestring (uiop:directory-files d)))))
     :walk-files   (lambda (rel) (host-walk-files (full rel) git-dir)))))
