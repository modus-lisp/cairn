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

(defun %cab-walk-wt (fs root reldir out)
  "Like %CAB-WALK but for a working tree: skip the .git subdirectory."
  (let ((dir (%cab-join root reldir))
        (base (string-right-trim "/" reldir)))
    (when (eq (cabinet:file-type fs dir) :directory)
      (dolist (e (cabinet:readdir fs dir) out)
        (unless (and (string= base "") (string= (car e) ".git"))
          (let ((child (if (string= base "") (car e)
                           (concatenate 'string base "/" (car e)))))
            (case (cdr e)
              (:directory (setf out (%cab-walk-wt fs root child out)))
              (t (push child out)))))))
    out))

(defun make-cabinet-worktree-backend (fs &key (root "/repo"))
  "A WT-BACKEND over the working tree in cabinet FS rooted at cabinet dir ROOT."
  (flet ((path (rel) (%cab-join root rel)))
    (make-wt-backend
     :read-file    (lambda (rel) (cabinet:read-file fs (path rel)))
     :write-file   (lambda (rel bytes exec)
                     (let ((p (path rel))) (%cab-ensure-parent fs p)
                       (cabinet:write-file fs p (coerce bytes '(simple-array (unsigned-byte 8) (*))))
                       (when exec (cabinet:chmod fs p #o755)) t))
     :make-symlink (lambda (rel target)
                     (let ((p (path rel))) (%cab-ensure-parent fs p)
                       (when (member (cabinet:file-type fs p) '(:file :symlink :directory))
                         (cabinet:unlink fs p))
                       (cabinet:symlink fs target p) t))
     :read-symlink (lambda (rel) (cabinet:readlink fs (path rel)))
     :delete-file  (lambda (rel)
                     (let ((p (path rel)))
                       (when (member (cabinet:file-type fs p) '(:file :symlink))
                         (cabinet:unlink fs p))
                       t))
     :exists-p     (lambda (rel) (and (cabinet:exists-p fs (path rel)) t))
     :lstat        (lambda (rel)
                     (handler-case
                         (let ((s (cabinet:stat fs (path rel))))
                           (make-wt-stat
                            :type (case (cabinet:stat-type s) (:directory :dir)
                                        (:symlink :symlink) (t :file))
                            :mode (cabinet:stat-mode s) :size (cabinet:stat-size s)
                            :mtime (cabinet:stat-mtime s) :ctime (cabinet:stat-ctime s)
                            :ino (cabinet:stat-ino s)))
                       (cabinet:cabinet-error () nil)))
     :walk         (lambda (reldir) (nreverse (%cab-walk-wt fs root reldir '()))))))

(defun open-cabinet-repository (fs &key (root "/repo") bare init)
  "Open a cairn repository whose store lives in the cabinet filesystem FS.  A
   non-bare repo (the default) keeps its working tree at ROOT and its git store at
   ROOT/.git; a :BARE repo keeps only the store, at ROOT.  With :INIT, first lay
   down a fresh empty repository.  Returns the repository."
  (let* ((root (string-right-trim "/" root))
         (git-dir (if bare root (concatenate 'string root "/.git")))
         (repo (make-repository
                :backend (make-cabinet-backend fs :root git-dir)
                :worktree (unless bare (make-cabinet-worktree-backend fs :root root))
                :git-dir git-dir :path (unless bare root))))
    (when init (init-repository repo :bare bare))
    (setf (repo-format repo) (detect-object-format repo))
    repo))
