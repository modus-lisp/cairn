;;;; pagetree-backend.lisp — a cairn git store directly on pagetree KV, with
;;;; operation-level atomic commit/push.
;;;;
;;;;   sbcl --non-interactive --load inspect/pagetree-backend.lisp
;;;;
;;;; Validates the direct backend the same three ways as the cabinet one (content-
;;;; addressing self-verify, cross-backend equivalence vs a git-proven host repo,
;;;; real git fsck on a materialized mirror, plus a persistence round trip) — and
;;;; then the reason it exists: ATOMICITY.  A push/apply that fails part-way
;;;; leaves the store completely unchanged (one pagetree txn), whereas the same
;;;; failure on the per-write cabinet backend leaves objects half-written.

(require :asdf)
(require :sb-posix)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :cairn/pagetree) (asdf:load-system :cairn/cabinet)))

(defvar *dir* "/tmp/cairn-pttest")
(defvar *checks* 0) (defvar *fails* 0)
(defun check (ok fmt &rest args)
  (incf *checks*) (unless ok (incf *fails*) (format t "  FAIL: ~?~%" fmt args)))
(defun sh (&rest a) (uiop:run-program a :output :string :error-output :string :ignore-error-status t))
(defun git (dir &rest a) (apply #'sh "git" "-C" dir a))

;;; ---- seed: two commits, so there's a delta to apply ------------------------
(sh "rm" "-rf" *dir*)
(ensure-directories-exist (format nil "~a/seed/" *dir*))
(let ((seed (format nil "~a/seed" *dir*)))
  (git seed "init" "-q" "-b" "master")
  (with-open-file (s (format nil "~a/README.md" seed) :direction :output) (write-string "one" s))
  (git seed "add" "-A") (git seed "-c" "user.name=s" "-c" "user.email=s@s" "commit" "-q" "-m" "c1")
  (with-open-file (s (format nil "~a/README.md" seed) :direction :output :if-exists :supersede)
    (write-string "one two" s))
  (with-open-file (s (format nil "~a/NEW.txt" seed) :direction :output) (write-string "new file" s))
  (git seed "add" "-A") (git seed "-c" "user.name=s" "-c" "user.email=s@s" "commit" "-q" "-m" "c2"))

(defvar *host* (cairn:open-repository (format nil "~a/seed" *dir*)))
(defvar *c2* (cairn:head-commit *host*))
(defvar *c1* (first (cairn:commit-parents (cairn:parse-commit (cairn:object-data *host* *c2*)))))
(defun reachable (repo sha) (loop for k being the hash-key of (cairn::reachable-objects repo sha) collect k))
(defvar *r1* (reachable *host* *c1*))
(defvar *r2* (reachable *host* *c2*))
(defvar *delta* (set-difference *r2* *r1* :test #'string=))
(defvar *pack1* (cairn:write-packfile *host* *r1*))
(defvar *packd* (cairn:write-packfile *host* *delta*))
(format t "~&seed: c1 ~a (~d obj), c2 ~a (~d obj), delta ~d obj~%"
        (subseq *c1* 0 8) (length *r1*) (subseq *c2* 0 8) (length *r2*) (length *delta*))

(defun have (repo sha) (cairn::have-object-p repo sha))
(defun seed-c1 (repo)                      ; one atomic "receive C1"
  (cairn:with-store-transaction (repo)
    (cairn:index-pack repo *pack1*) (setf (cairn::repo-packs repo) :unloaded)
    (cairn:update-ref repo "refs/heads/master" *c1*)))

;;; ---- the direct pagetree store ---------------------------------------------
(defvar *pt* (format nil "~a/direct.pt" *dir*))
(defvar *store* (pagetree:open-store *pt*))
(defvar *repo* (cairn:open-pagetree-repository *store* :init t))
(seed-c1 *repo*)
(check (string= (cairn:head-commit *repo*) *c1*) "seeded head = c1")
(check (have *repo* *c1*) "c1 present")
(check (not (have *repo* *c2*)) "c2 absent before apply")

(format t "~%[atomic apply] one txn: index the delta pack + move the ref~%")
(cairn:with-store-transaction (*repo*)
  (cairn:index-pack *repo* *packd*) (setf (cairn::repo-packs *repo*) :unloaded)
  (cairn:update-ref *repo* "refs/heads/master" *c2*))
(check (string= (cairn:head-commit *repo*) *c2*) "head advanced to c2")
(dolist (sha *r2*)
  (multiple-value-bind (th ch) (cairn:read-object *host* sha)
    (multiple-value-bind (tc cc) (cairn:read-object *repo* sha)
      (check (and (eq th tc) (equalp ch cc)) "~a cross-backend equal" (subseq sha 0 8))
      (check (string= sha (cairn:hash-object tc cc)) "~a rehash = sha" (subseq sha 0 8)))))

;;; ---- ATOMICITY: a failed apply leaves the store untouched ------------------
(format t "~%[atomicity] a push that fails mid-way rolls back completely~%")
(let* ((pt2 (format nil "~a/abort.pt" *dir*))
       (store2 (pagetree:open-store pt2))
       (repo2 (cairn:open-pagetree-repository store2 :init t)))
  (seed-c1 repo2)
  (handler-case
      (cairn:with-store-transaction (repo2)
        (cairn:index-pack repo2 *packd*) (setf (cairn::repo-packs repo2) :unloaded)
        (error "simulated crash after writing objects, before the ref move")
        (cairn:update-ref repo2 "refs/heads/master" *c2*))
    (error () (format t "  (injected failure caught)~%")))
  (setf (cairn::repo-packs repo2) :unloaded)
  (check (string= (cairn:head-commit repo2) *c1*) "direct: head STILL c1 after abort")
  (check (not (have repo2 *c2*)) "direct: c2 objects ABSENT after abort (all-or-nothing)")
  (pagetree:close-store store2))

;;; ---- contrast: the per-write cabinet backend leaves a partial write --------
(format t "~%[contrast] same failure on the non-transactional cabinet backend~%")
(let* ((cabpt (format nil "~a/cab.pt" *dir*))
       (fs (cabinet:format-fs cabpt))
       (cabrepo (cairn:open-cabinet-repository fs :root "/r.git" :bare t :init t)))
  (seed-c1 cabrepo)
  (handler-case
      (cairn:with-store-transaction (cabrepo)   ; a no-op wrapper for cabinet
        (cairn:index-pack cabrepo *packd*) (setf (cairn::repo-packs cabrepo) :unloaded)
        (error "same simulated crash")
        (cairn:update-ref cabrepo "refs/heads/master" *c2*))
    (error () (format t "  (injected failure caught)~%")))
  (setf (cairn::repo-packs cabrepo) :unloaded)
  (check (string= (cairn:head-commit cabrepo) *c1*) "cabinet: head still c1 (ref move never ran)")
  (check (have cabrepo *c2*) "cabinet: c2 objects PRESENT after failure — orphaned partial write")
  (format t "  => direct-pagetree rolled the objects back; cabinet left them orphaned~%")
  (cabinet:unmount fs))

;;; ---- real-git bridge + persistence (the applied direct store) --------------
(format t "~%[git fsck] materialize the direct store -> host dir, run real git~%")
(let* ((mirror (format nil "~a/mirror.git" *dir*))
       (host2 (cairn:make-repository-on-backend
               (cairn:make-host-backend (uiop:ensure-directory-pathname mirror))
               :git-dir (uiop:ensure-directory-pathname mirror))))
  (cairn:init-repository host2)
  (dolist (sha *r2*) (multiple-value-bind (ty c) (cairn:read-object *repo* sha)
                       (cairn:write-object host2 ty c)))
  (cairn:update-ref host2 "refs/heads/master" *c2*)
  (let ((fsck (git mirror "fsck" "--strict")))
    (check (not (search "error" (string-downcase fsck))) "git fsck: ~a" fsck)
    (format t "  git fsck: ~a~%" (if (string= "" (string-trim '(#\Newline) fsck)) "clean" fsck))))

(format t "~%[persistence] close the pagetree file, reopen, re-verify~%")
(pagetree:close-store *store*)
(let* ((store3 (pagetree:open-store *pt*))
       (repo3 (cairn:open-pagetree-repository store3)))
  (check (string= (cairn:head-commit repo3) *c2*) "reopened head = c2")
  (dolist (sha *r2*)
    (multiple-value-bind (ty c) (cairn:read-object repo3 sha)
      (check (string= sha (cairn:hash-object ty c)) "~a rehash after reopen" (subseq sha 0 8))))
  (pagetree:close-store store3))

(format t "~%----------------------------------~%")
(format t "checks: ~d   failures: ~d   => ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
