;;;; backend-cabinet.lisp — a cairn storage backend that lives in a cabinet.
;;;;
;;;; This is the opt-in bridge (system :cairn/cabinet): instead of the host
;;;; filesystem, a cairn repository's whole git store — objects, refs, index,
;;;; packs, config, HEAD — is kept inside a cabinet filesystem, which itself
;;;; lives in a single pagetree file.  So a git repository becomes one portable,
;;;; crash-atomic file on our own stack (natrium → pagetree → cabinet → cairn),
;;;; and cairn runs where there is no host filesystem at all (modus).
;;;;
;;;; It simply supplies cabinet-backed closures for the FS-BACKEND protocol in
;;;; fs-backend.lisp; nothing in cairn's core changes.

(in-package #:cairn)

(defun %cab-join (root rel)
  "Absolute cabinet path for the git-dir-relative REL under ROOT (\"\" = the
   cabinet root)."
  (let ((rel (string-left-trim "/" rel)))
    (if (or (null root) (string= root "") (string= root "/"))
        (concatenate 'string "/" rel)
        (concatenate 'string (string-right-trim "/" root) "/" rel))))

(defun %cab-ensure-parent (fs path)
  "Make PATH's parent directory (and ancestors) exist."
  (let ((slash (position #\/ path :from-end t)))
    (when (and slash (plusp slash))
      (cabinet:make-directories fs (subseq path 0 slash)))))

(defun %cab-walk (fs root reldir out)
  "Push git-dir-relative paths of every file anywhere under RELDIR onto OUT."
  (let ((dir (%cab-join root reldir))
        (base (string-right-trim "/" reldir)))
    (when (eq (cabinet:file-type fs dir) :directory)
      (dolist (e (cabinet:readdir fs dir) out)
        (let ((child (if (string= base "") (car e)
                         (concatenate 'string base "/" (car e)))))
          (case (cdr e)
            (:directory (setf out (%cab-walk fs root child out)))
            (t (push child out))))))
    out))

(defun make-cabinet-backend (fs &key (root ""))
  "An FS-BACKEND whose git store lives in the mounted cabinet filesystem FS,
   rooted at the cabinet directory ROOT (default the cabinet root).  Pair with
   MAKE-REPOSITORY-ON-BACKEND / OPEN-CABINET-REPOSITORY."
  (flet ((path (rel) (%cab-join root rel)))
    (make-fs-backend
     :read-bytes   (lambda (rel)
                     (let ((p (path rel)))
                       (when (eq (cabinet:file-type fs p) :file) (cabinet:read-file fs p))))
     :read-string  (lambda (rel)
                     (let ((p (path rel)))
                       (when (eq (cabinet:file-type fs p) :file)
                         (cabinet:utf8->string (cabinet:read-file fs p)))))
     :write-bytes  (lambda (rel bytes)
                     (let ((p (path rel))) (%cab-ensure-parent fs p)
                       (cabinet:write-file fs p (coerce bytes '(simple-array (unsigned-byte 8) (*)))) t))
     :write-string (lambda (rel text)
                     (let ((p (path rel))) (%cab-ensure-parent fs p)
                       (cabinet:write-file fs p (cabinet:string->utf8 text)) t))
     :exists-p     (lambda (rel) (and (cabinet:exists-p fs (path rel)) t))
     :delete-file  (lambda (rel)
                     (let ((p (path rel)))
                       (when (member (cabinet:file-type fs p) '(:file :symlink))
                         (cabinet:unlink fs p))
                       t))
     :dir-exists-p (lambda (rel) (eq (cabinet:file-type fs (path rel)) :directory))
     :list-files   (lambda (rel)
                     (let ((p (path rel)))
                       (when (eq (cabinet:file-type fs p) :directory)
                         (loop for e in (cabinet:readdir fs p)
                               when (eq (cdr e) :file) collect (car e)))))
     :walk-files   (lambda (rel) (nreverse (%cab-walk fs root rel '()))))))

(defun open-cabinet-repository (fs &key (root "") init)
  "Open (or, with :INIT, create a fresh bare) cairn repository whose git store is
   the cabinet filesystem FS under ROOT.  Returns the repository."
  (let ((repo (make-repository :backend (make-cabinet-backend fs :root root)
                               :path root :git-dir root)))
    (when init (init-repository repo))
    (setf (repo-format repo) (detect-object-format repo))
    repo))
