;;;; cabinet-worktree.lisp — a full working tree (checkout/edit/commit) in a
;;;; pagetree file, via the cabinet worktree backend.
;;;;
;;;;   sbcl --non-interactive --load inspect/cabinet-worktree.lisp
;;;;
;;;; Milestone 2: not just the .git store but the WORKING TREE lives in a cabinet
;;;; (a pagetree file) — files with modes (exec) and symlinks, plus the index.
;;;; Proven by: checkout fidelity (re-hash the checked-out tree -> identical tree
;;;; SHA, so content+exec-bit+symlink-target all round-tripped), a clean status,
;;;; an identical-commit-SHA cross-check against a host repo (same edit + author +
;;;; time -> same SHA, so worktree hashing matches git exactly), a real `git fsck`
;;;; on a materialized mirror, and a persistence round trip.

(require :asdf)
(require :sb-posix)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn/cabinet)))

(defvar *dir* "/tmp/cairn-cabwt")
(defvar *checks* 0) (defvar *fails* 0)
(defun check (ok fmt &rest args)
  (incf *checks*) (unless ok (incf *fails*) (format t "  FAIL: ~?~%" fmt args)))
(defun sh (&rest a) (uiop:run-program a :output :string :error-output :string :ignore-error-status t))
(defun git (dir &rest a) (apply #'sh "git" "-C" dir a))
(defun b= (x y) (and (= (length x) (length y)) (every #'= x y)))

;;; ---- a real seed repo with an exec file and a symlink ----------------------
(sh "rm" "-rf" *dir*)
(ensure-directories-exist (format nil "~a/lib/" (format nil "~a/seed" *dir*)))
(let ((seed (format nil "~a/seed" *dir*)))
  (git seed "init" "-q" "-b" "master")
  (with-open-file (s (format nil "~a/README.md" seed) :direction :output) (write-string "# demo" s))
  (with-open-file (s (format nil "~a/lib/util.txt" seed) :direction :output) (write-string "utils" s))
  (with-open-file (s (format nil "~a/run.sh" seed) :direction :output) (write-string "#!/bin/sh~%echo hi" s))
  (sb-posix:chmod (format nil "~a/run.sh" seed) #o755)
  (sb-posix:symlink "README.md" (format nil "~a/link" seed))       ; a symlink
  (git seed "add" "-A")
  (git seed "-c" "user.name=s" "-c" "user.email=s@s" "commit" "-q" "-m" "first"))

(defvar *host* (cairn:open-repository (format nil "~a/seed" *dir*)))
(defvar *head* (cairn:head-commit *host*))
(defvar *head-tree* (cairn:commit-tree (cairn:parse-commit (cairn:object-data *host* *head*))))
(defvar *shas* (loop for k being the hash-key of (cairn::reachable-objects *host* *head*) collect k))
(format t "~&seed head ~a tree ~a (~d objects, incl exec + symlink)~%"
        (subseq *head* 0 8) (subseq *head-tree* 0 8) (length *shas*))

;;; ---- a NON-bare cairn repo whose worktree lives in a pagetree file ----------
(defvar *pt* (format nil "~a/repo.pt" *dir*))
(defvar *fs* (cabinet:format-fs *pt*))
(defvar *cab* (cairn:open-cabinet-repository *fs* :root "/repo" :init t))   ; non-bare
;; seed the object store + branch (as if just fetched)
(cairn:index-pack *cab* (cairn:write-packfile *host* *shas*))
(cairn:update-ref *cab* "refs/heads/master" *head*)

(format t "~%[checkout] materialize HEAD into the cabinet working tree~%")
(let ((n (cairn:checkout *cab*)))
  (format t "  checked out ~d files~%" n)
  (check (= n 4) "file count ~d (want 4)" n))

;; the working tree is really inside the cabinet — read it back through cabinet
(check (b= (cabinet:read-file *fs* "/repo/README.md") (cabinet:string->utf8 "# demo")) "README content")
(check (eq (cabinet:file-type *fs* "/repo/link") :symlink) "link is a symlink")
(check (string= (cabinet:readlink *fs* "/repo/link") "README.md") "symlink target")
(check (logtest (cabinet:stat-mode (cabinet:stat *fs* "/repo/run.sh")) #o111) "run.sh keeps exec bit")

(format t "~%[status] clean right after checkout~%")
(multiple-value-bind (st un unt unm) (cairn:status *cab*)
  (check (and (null st) (null un) (null unt) (null unm)) "status not clean: ~s ~s ~s ~s" st un unt unm))

(format t "~%[fidelity] re-hash the checked-out tree -> identical tree SHA~%")
(apply #'cairn:add *cab* (cairn::walk-worktree *cab*))
(let ((tree (cairn:write-tree *cab*)))
  (check (string= tree *head-tree*) "re-hashed tree ~a vs ~a" (subseq tree 0 8) (subseq *head-tree* 0 8)))

;;; ---- identical-commit-SHA cross-check: cabinet edit == host edit ------------
(format t "~%[cross-check] same edit+author+time on host and in cabinet -> same commit SHA~%")
(flet ((commit-args () '(:author "t <t@t>" :committer "t <t@t>" :time 1000000000 :timezone "+0000")))
  ;; host: edit README, add, commit
  (with-open-file (s (format nil "~a/seed/README.md" *dir*) :direction :output :if-exists :supersede)
    (write-string "# demo EDITED" s))
  (cairn:add *host* "README.md")
  (let ((host-sha (apply #'cairn:commit *host* :message "edit" (commit-args))))
    ;; cabinet: the same edit through the cabinet worktree
    (cabinet:write-file *fs* "/repo/README.md" (cabinet:string->utf8 "# demo EDITED"))
    (cairn:add *cab* "README.md")
    (let ((cab-sha (apply #'cairn:commit *cab* :message "edit" (commit-args))))
      (check (string= host-sha cab-sha) "commit SHA host ~a vs cabinet ~a"
             (subseq host-sha 0 8) (subseq cab-sha 0 8))
      (format t "  both produced ~a~%" (subseq cab-sha 0 8))
      (setf *head* cab-sha))))

;;; ---- real-git bridge: materialize the cabinet repo, git fsck ---------------
(format t "~%[git fsck] materialize cabinet repo -> host dir, run real git~%")
(defvar *new-shas* (loop for k being the hash-key of (cairn::reachable-objects *cab* *head*) collect k))
(let* ((mirror (format nil "~a/mirror.git" *dir*))
       (host2 (cairn:make-repository-on-backend
               (cairn:make-host-backend (uiop:ensure-directory-pathname mirror))
               :git-dir (uiop:ensure-directory-pathname mirror))))
  (cairn:init-repository host2)
  (dolist (sha *new-shas*)
    (multiple-value-bind (type content) (cairn:read-object *cab* sha)
      (cairn:write-object host2 type content)))
  (cairn:update-ref host2 "refs/heads/master" *head*)
  (let ((fsck (git mirror "fsck" "--strict")))
    (check (not (search "error" (string-downcase fsck))) "git fsck: ~a" fsck)
    (format t "  git fsck: ~a~%" (if (string= "" (string-trim '(#\Newline) fsck)) "clean" fsck))
    (format t "  git ls-tree:~%~a" (git mirror "ls-tree" "-r" "HEAD"))))

;;; ---- persistence: remount the pagetree file, status still clean -------------
(format t "~%[persistence] remount the pagetree file, status still clean~%")
(cabinet:unmount *fs*)
(let* ((fs2 (cabinet:mount *pt*))
       (cab2 (cairn:open-cabinet-repository fs2 :root "/repo")))
  (multiple-value-bind (st un unt unm) (cairn:status cab2)
    (check (and (null st) (null un) (null unt) (null unm)) "post-remount status: ~s ~s ~s ~s" st un unt unm))
  (check (string= (cairn:head-commit cab2) *head*) "post-remount head")
  (cabinet:unmount fs2))

(format t "~%----------------------------------~%")
(format t "checks: ~d   failures: ~d   => ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
