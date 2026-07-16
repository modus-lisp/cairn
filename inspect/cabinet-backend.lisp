;;;; cabinet-backend.lisp — a cairn repository living entirely in a pagetree file.
;;;;
;;;;   sbcl --non-interactive --load inspect/cabinet-backend.lisp
;;;;
;;;; The real-git differential oracle can't inspect a repo stored inside a
;;;; pagetree file, so this validates the cabinet backend three ways, in
;;;; increasing strength:
;;;;   1. content-addressing — every object rehashes to its own name (backend-
;;;;      agnostic; exactly what `git fsck` checks), so the cabinet store is
;;;;      self-verifying with no host git involved.
;;;;   2. cross-backend equivalence — the SAME objects read byte-identical from a
;;;;      git-proven host repo and from the cabinet repo (host == git, so
;;;;      cabinet == git by transitivity).
;;;;   3. real-git bridge — materialize the cabinet repo out to a host dir and run
;;;;      actual `git fsck --strict` on it.
;;;; Plus a persistence round trip: unmount the pagetree file, remount, re-verify.

(require :asdf)
(require :sb-posix)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :cairn/cabinet)))

(defvar *dir* "/tmp/cairn-cabtest")
(defvar *checks* 0) (defvar *fails* 0)
(defun check (ok fmt &rest args)
  (incf *checks*) (unless ok (incf *fails*) (format t "  FAIL: ~?~%" fmt args)))
(defun sh (&rest args) (uiop:run-program args :output :string :error-output :string
                                              :ignore-error-status t))
(defun git (dir &rest args) (apply #'sh "git" "-C" dir args))

;;; ---- a real seed repo (git makes it, so it is git-correct) ------------------
(sh "rm" "-rf" *dir*)
(ensure-directories-exist (format nil "~a/" *dir*))
(let ((seed (format nil "~a/seed" *dir*)))
  (ensure-directories-exist (format nil "~a/lib/" seed))
  (git seed "init" "-q" "-b" "master")
  (with-open-file (s (format nil "~a/README.md" seed) :direction :output) (write-string "# demo" s))
  (with-open-file (s (format nil "~a/lib/util.txt" seed) :direction :output) (write-string "utils" s))
  (git seed "add" "-A")
  (git seed "-c" "user.name=s" "-c" "user.email=s@s" "commit" "-q" "-m" "first")
  (with-open-file (s (format nil "~a/README.md" seed) :direction :output :if-exists :supersede)
    (write-string "# demo v2" s))
  (git seed "add" "-A")
  (git seed "-c" "user.name=s" "-c" "user.email=s@s" "commit" "-q" "-m" "second"))

(defvar *host* (cairn:open-repository (format nil "~a/seed" *dir*)))
(defvar *head* (cairn:head-commit *host*))
(defvar *shas* (let ((h (cairn::reachable-objects *host* *head*)))
                 (loop for k being the hash-key of h collect k)))
(format t "~&seed: head ~a, ~d reachable objects~%" (subseq *head* 0 8) (length *shas*))

;;; ---- move the whole object set into a cabinet in a pagetree file ------------
(defvar *pt* (format nil "~a/repo.pt" *dir*))
(defun verify-store (repo label)
  "Every reachable object reads back byte-identical to the host AND rehashes to
   its own SHA (git-correct)."
  (dolist (sha *shas*)
    (multiple-value-bind (th ch) (cairn:read-object *host* sha)
      (multiple-value-bind (tc cc) (cairn:read-object repo sha)
        (check (eq th tc) "~a: ~a type ~a/~a" label (subseq sha 0 8) th tc)
        (check (equalp ch cc) "~a: ~a bytes differ" label (subseq sha 0 8))
        (check (string= sha (cairn:hash-object tc cc)) "~a: ~a rehash mismatch" label (subseq sha 0 8)))))
  (check (string= *head* (cairn:head-commit repo)) "~a: head-commit" label)
  (check (equal (cairn:list-refs repo) (list (cons "refs/heads/master" *head*)))
         "~a: list-refs ~s" label (cairn:list-refs repo)))

(let* ((pack (cairn:write-packfile *host* *shas*))
       (fs (cabinet:format-fs *pt*))
       (cab (cairn:open-cabinet-repository fs :root "/r.git" :bare t :init t)))
  (format t "built pack: ~d bytes; writing it into the cabinet…~%" (length pack))
  (cairn:index-pack cab pack)
  (cairn:update-ref cab "refs/heads/master" *head*)
  (format t "~%[1+2] self-verify + cross-backend equivalence (live cabinet)~%")
  (verify-store cab "live")
  (cabinet:unmount fs))

;;; ---- persistence: reopen the pagetree file and re-verify --------------------
(format t "~%[persistence] remount the pagetree file, re-verify~%")
(let* ((fs (cabinet:mount *pt*))
       (cab (cairn:open-cabinet-repository fs :root "/r.git" :bare t)))
  (verify-store cab "remount")

  ;; ---- [3] real-git bridge: materialize out to a host dir, git fsck ----------
  (format t "~%[3] materialize cabinet repo -> host dir, run real git fsck~%")
  (let* ((mirror (format nil "~a/mirror.git" *dir*))
         (host2 (cairn:make-repository-on-backend
                 (cairn:make-host-backend (uiop:ensure-directory-pathname mirror))
                 :git-dir (uiop:ensure-directory-pathname mirror))))
    (cairn:init-repository host2)
    (dolist (sha *shas*)
      (multiple-value-bind (type content) (cairn:read-object cab sha)
        (cairn:write-object host2 type content)))
    (cairn:update-ref host2 "refs/heads/master" *head*)
    (let ((fsck (git mirror "fsck" "--strict"))
          (top (string-trim '(#\Newline) (git mirror "rev-parse" "refs/heads/master"))))
      (check (string= top *head*) "mirror head ~a vs ~a" top *head*)
      (check (not (search "error" (string-downcase fsck))) "git fsck: ~a" fsck)
      (format t "  git fsck: ~a~%" (if (string= "" (string-trim '(#\Newline) fsck)) "clean" fsck))
      (format t "  git log:~%~a" (git mirror "log" "--oneline"))))
  (cabinet:unmount fs))

(format t "~%----------------------------------~%")
(format t "checks: ~d   failures: ~d   => ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
