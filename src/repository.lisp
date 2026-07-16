;;;; repository.lisp — the repository handle and object lookup.

(in-package #:cairn)

(defstruct (repository (:conc-name repo-))
  path git-dir backend worktree (packs :unloaded) (format :sha1))

(defun detect-object-format (repo)
  "Read [extensions] objectformat from the store's config; :sha256 or :sha1."
  (let ((cfg (ignore-errors (fs-read-string repo "config"))))
    (if (and cfg (search "objectformat = sha256" cfg)) :sha256 :sha1)))

(defmacro with-oid ((repo) &body body)
  "Evaluate BODY with *OID* bound to REPO's object format."
  `(let ((*oid* (repo-format ,repo))) ,@body))

(defun open-repository (path)
  "Open the git repository at PATH on the host filesystem (which must contain a
   .git directory, or be a bare repository).  See MAKE-REPOSITORY-ON-BACKEND for
   opening a repository whose store lives elsewhere (e.g. in a cabinet)."
  (let* ((base (uiop:ensure-directory-pathname path))
         (dotgit (merge-pathnames ".git/" base)))
    (flet ((mk (git-dir worktree)
             (let ((repo (make-repository-on-backend (make-host-backend git-dir)
                                                     :path (or worktree git-dir) :git-dir git-dir)))
               (when worktree
                 (setf (repo-worktree repo) (make-host-worktree-backend worktree)))
               repo)))
      (cond ((probe-file (merge-pathnames "HEAD" dotgit)) (mk dotgit base))
            ((probe-file (merge-pathnames "HEAD" base)) (mk base nil))    ; bare repo
            (t (error "cairn: not a git repository: ~a" path))))))

(defun make-repository-on-backend (backend &key path git-dir)
  "Build a repository whose entire git store is read and written through BACKEND
   (a FS-BACKEND).  PATH/GIT-DIR are informational for the host backend and may
   be NIL for others.  The object format is detected from the store's config."
  (let ((repo (make-repository :path path :git-dir git-dir :backend backend)))
    (setf (repo-format repo) (detect-object-format repo))
    repo))

(defun init-repository (repo &key (head "refs/heads/master") (bare t))
  "Lay down a minimal git store through REPO's backend: a symbolic HEAD pointing
   at HEAD and a stub config.  Objects and refs directories are created on demand
   by later writes.  Returns REPO."
  (fs-write-string repo "HEAD" (format nil "ref: ~a~%" head))
  (fs-write-string repo "config"
                   (format nil "[core]~%	repositoryformatversion = 0~%	bare = ~a~%"
                           (if bare "true" "false")))
  repo)

(defmacro with-repository ((var path) &body body)
  `(let ((,var (open-repository ,path))) ,@body))

(defun read-object (repo sha)
  "Return (values TYPE-KEYWORD CONTENT-BYTES) for the object named by the hex
   SHA.  Looks in the loose object store, then packfiles."
  (with-oid (repo)
    (let ((loose (fs-read-bytes repo (loose-object-relpath sha))))
      (if loose
          (parse-object (zlib-decompress loose))
          (read-pack-object repo sha)))))

(defun object-type (repo sha) (values (read-object repo sha)))
(defun object-data (repo sha) (nth-value 1 (read-object repo sha)))

(defun have-object-p (repo sha)
  "Is the object named by SHA present in REPO (loose or packed)?"
  (handler-case (progn (read-object repo sha) t) (error () nil)))

(defun rel-worktree-path (root relpath)
  "A wildcard-safe absolute pathname for RELPATH (forward slashes) under the
   worktree ROOT (a directory pathname).  SBCL parses [ ] * ? in a namestring as
   glob patterns, so we build the pathname from literal name components instead —
   real repos have files like `[Content_Types].xml`."
  (let ((parts (remove "" (uiop:split-string relpath :separator "/") :test #'string=)))
    (make-pathname :directory (append (pathname-directory root) (butlast parts))
                   :name (car (last parts)) :type nil :defaults root)))

(defun worktree-path (repo relpath) (rel-worktree-path (repo-path repo) relpath))

(defun native (pathname)
  "The OS-native path string (no CL namestring escaping) — for sb-posix calls."
  (sb-ext:native-namestring pathname))

(defun make-host-worktree-backend (root)
  "A WT-BACKEND over the real host working tree rooted at ROOT (a directory
   pathname): the prior sb-posix/uiop behavior behind the worktree protocol."
  (labels ((full (rel) (rel-worktree-path root rel))
           (nat (rel) (native (full rel))))
    (make-wt-backend
     :read-file    (lambda (rel) (slurp-bytes (full rel)))
     :write-file   (lambda (rel bytes exec)
                     (write-bytes (full rel) bytes)
                     (when exec (sb-posix:chmod (nat rel) #o755)) t)
     :make-symlink (lambda (rel target)
                     (ensure-directories-exist (full rel))
                     (uiop:delete-file-if-exists (full rel))
                     (sb-posix:symlink target (nat rel)) t)
     :read-symlink (lambda (rel) (sb-posix:readlink (nat rel)))
     :delete-file  (lambda (rel) (uiop:delete-file-if-exists (full rel)) t)
     :exists-p     (lambda (rel) (and (ignore-errors (sb-posix:lstat (nat rel))) t))
     :lstat        (lambda (rel)
                     (let ((st (ignore-errors (sb-posix:lstat (nat rel)))))
                       (when st
                         (let ((m (sb-posix:stat-mode st)))
                           (make-wt-stat
                            :type (cond ((= (logand m #o170000) #o120000) :symlink)
                                        ((= (logand m #o170000) #o040000) :dir)
                                        (t :file))
                            :mode m :size (sb-posix:stat-size st)
                            :mtime (sb-posix:stat-mtime st) :ctime (sb-posix:stat-ctime st)
                            :dev (sb-posix:stat-dev st) :ino (sb-posix:stat-ino st)
                            :uid (sb-posix:stat-uid st) :gid (sb-posix:stat-gid st))))))
     :walk         (lambda (reldir)
                     (let ((out '()) (base (uiop:ensure-directory-pathname (full reldir))))
                       (labels ((rel (prefix name) (if (string= prefix "") name
                                                       (concatenate 'string prefix "/" name)))
                                (walk (dir prefix)
                                  (dolist (f (uiop:directory-files dir))
                                    (push (rel prefix (file-namestring f)) out))
                                  (dolist (d (uiop:subdirectories dir))
                                    (let ((name (car (last (pathname-directory d)))))
                                      (unless (string= name ".git") (walk d (rel prefix name)))))))
                         (when (uiop:directory-exists-p base) (walk base "")))
                       (nreverse out))))))

(defun repo-loaded-packs (repo)
  "The repository's packfiles (opened + cached on first use)."
  (when (eq (repo-packs repo) :unloaded)
    (setf (repo-packs repo)
          (loop for name in (fs-list-files repo "objects/pack/")
                when (and (> (length name) 4)
                          (string= (subseq name (- (length name) 4)) ".idx"))
                  collect (let* ((base (subseq name 0 (- (length name) 4)))
                                 (idx (fs-read-bytes repo (format nil "objects/pack/~a.idx" base)))
                                 (pk  (fs-read-bytes repo (format nil "objects/pack/~a.pack" base))))
                            (open-pack-bytes idx pk name)))))
  (repo-packs repo))

(defun read-pack-object (repo sha)
  (dolist (pack (repo-loaded-packs repo)
                (error "cairn: object ~a not found (loose or packed)" sha))
    (let ((offset (pack-find-offset pack sha)))
      (when offset (return (pack-read-at pack offset repo))))))
