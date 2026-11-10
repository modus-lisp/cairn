;;;; fetch-http.lisp — incremental fetch + pull over smart HTTP, on a store that is not the host's.
;;;;
;;;;   sbcl --non-interactive --load inspect/fetch-http.lisp
;;;;
;;;; Real git serves a bare repo (`git http-backend' behind inspect/git-http-server.py, on
;;;; loopback, an ephemeral port).  cairn clones it over HTTP; real git pushes a second commit
;;;; (one file edited, one deleted, one added, the rest -- a 200 KB binary among them --
;;;; untouched); cairn pulls.  Asserted:
;;;;   1. THE STORE, NOT THE HOST'S FILES.  The pull runs on the repository reopened with a
;;;;      git-dir that does not exist: every read of .git must go through the store backend,
;;;;      as it must on a board's FAT32 store or a cabinet.  (fetch read refs and the remote's
;;;;      url with PROBE-FILE before this gate; pull could not run on any other store.)
;;;;   2. THE PACK STREAMS.  The fetch leaves one more pack and no incoming.pack.
;;;;   3. CHECKOUT WRITES WHAT CHANGED.  Exactly the edited and the added file are written; the
;;;;      deleted one is gone; real git calls the tree clean and the store sound.
;;;;   4. A SECOND PULL WRITES NOTHING and says up to date.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *dir* "/tmp/cairn-fetch-http")
(defvar *checks* 0) (defvar *fails* 0)
(defun check (ok fmt &rest args)
  (incf *checks*) (unless ok (incf *fails*) (format t "  FAIL: ~?~%" fmt args)))
(defun sh (&rest args) (uiop:run-program args :output :string :error-output :string
                                              :ignore-error-status t))
(defun git (dir &rest args) (apply #'sh "git" "-C" dir args))
(defun spit (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede) (write-string text s)))

(sh "rm" "-rf" *dir*)
(defvar *src* (format nil "~a/src" *dir*))
(defvar *srv* (format nil "~a/srv" *dir*))
(defvar *dest* (format nil "~a/clone/" *dir*))
(defvar *c2* nil)

;;; ---- a real repository, served by real git ------------------------------------------------
(spit (format nil "~a/README.md" *src*) "# v1")
(spit (format nil "~a/lib/a.txt" *src*) "kept")
(spit (format nil "~a/lib/b.txt" *src*) "deleted in C2")
(with-open-file (s (format nil "~a/big.bin" *src*) :direction :output :element-type '(unsigned-byte 8))
  (let ((r (sb-ext:seed-random-state 7)))
    (dotimes (i 200000) (write-byte (random 256 r) s))))
(git *src* "init" "-q" "-b" "main")
(git *src* "add" "-A")
(git *src* "-c" "user.name=s" "-c" "user.email=s@s" "commit" "-q" "-m" "C1")
(ensure-directories-exist (format nil "~a/" *srv*))
(sh "git" "clone" "-q" "--bare" *src* (format nil "~a/r.git" *srv*))

(defvar *server*
  (uiop:launch-program (list "python3" "-I" (namestring (asdf:system-relative-pathname :cairn "inspect/git-http-server.py"))
                             *srv*)
                       :output :stream :error-output nil))
(defvar *port* (parse-integer (read-line (uiop:process-info-output *server*))))
(defvar *url* (format nil "http://127.0.0.1:~d/r.git" *port*))
(format t "~&serving ~a~%" *url*)

(unwind-protect
     (progn
       ;; ---- cairn clones C1 over HTTP ----------------------------------------------------------
       (let ((*standard-output* (make-broadcast-stream))) (cairn:clone *url* *dest*))
       (let ((c1 (git *src* "rev-parse" "HEAD")))
         (check (string= (string-trim '(#\Newline) c1) (cairn:head-commit (cairn:open-repository *dest*)))
                "clone: HEAD is C1"))

       ;; ---- real git pushes C2 -----------------------------------------------------------------
       (spit (format nil "~a/README.md" *src*) "# v2")
       (git *src* "rm" "-q" "lib/b.txt")
       (spit (format nil "~a/new/c.txt" *src*) "added in C2")
       (git *src* "add" "-A")
       (git *src* "-c" "user.name=s" "-c" "user.email=s@s" "commit" "-q" "-m" "C2")
       (git *src* "push" "-q" (format nil "~a/r.git" *srv*) "main")
       (setf *c2* (string-trim '(#\Newline) (git *src* "rev-parse" "HEAD")))

       ;; ---- the same repository, reopened so that only its store can reach it -------------------
       (let* ((git-dir (merge-pathnames ".git/" *dest*))
              (packs-before (count-if (lambda (p) (string= (pathname-type p) "pack"))
                                      (directory (merge-pathnames "objects/pack/*.*" git-dir))))
              (written '())
              (repo (cairn::make-repository-on-backend (cairn::make-host-backend git-dir)
                                                       :path *dest* :git-dir #p"/nonexistent-cairn-git-dir/"))
              (wt (cairn::copy-wt-backend (cairn::make-host-worktree-backend (pathname *dest*)))))
         (let ((open (cairn::wtb-open-output wt)) (whole (cairn::wtb-write-file wt)))
           (setf (cairn::wtb-open-output wt) (lambda (rel exec) (push rel written) (funcall open rel exec))
                 (cairn::wtb-write-file wt) (lambda (rel bytes exec) (push rel written) (funcall whole rel bytes exec))))
         (setf (cairn::repo-worktree repo) wt)
         (check (string= (cairn:remote-url repo) *url*) "remote-url read through the store: ~s" (cairn:remote-url repo))

         ;; 1-3: pull
         (let ((r (let ((*standard-output* (make-broadcast-stream))) (cairn:pull repo))))
           (check (eq r :fast-forward) "pull: a fast-forward, got ~s" r))
         (check (string= (cairn:head-commit repo) *c2*) "pull: HEAD is C2")
         (check (string= (cairn::resolve-ref repo "refs/remotes/origin/main") *c2*) "origin/main is C2")
         (check (equal (sort (copy-list written) #'string<) '("README.md" "new/c.txt"))
                "checkout wrote ~s, not just README.md and new/c.txt" (reverse written))
         (check (not (probe-file (format nil "~alib/b.txt" *dest*))) "lib/b.txt deleted")
         (check (string= (uiop:read-file-string (format nil "~aREADME.md" *dest*)) "# v2") "README.md is v2")
         (let ((packs (directory (merge-pathnames "objects/pack/*.*" git-dir))))
           (check (= (count-if (lambda (p) (string= (pathname-type p) "pack")) packs) (1+ packs-before))
                  "one more pack after the fetch")
           (check (notany (lambda (p) (search "incoming" (namestring p))) packs) "no incoming.pack left"))
         (check (string= (git *dest* "status" "--porcelain") "") "git status: clean, got ~s"
                (git *dest* "status" "--porcelain"))
         (check (string= (git *dest* "fsck" "--strict") "") "git fsck --strict: ~s" (git *dest* "fsck" "--strict"))

         ;; 4: again -- nothing to fetch, nothing to write
         (setf written '())
         (let ((r (let ((*standard-output* (make-broadcast-stream))) (cairn:pull repo))))
           (check (eq r :up-to-date) "second pull: up to date, got ~s" r))
         (check (null written) "second pull wrote ~s" written)))
  (uiop:terminate-process *server*)
  (uiop:wait-process *server*))

(format t "~&fetch-http: ~d checks, ~d failed~%" *checks* *fails*)
(sh "rm" "-rf" *dir*)
(uiop:quit (if (zerop *fails*) 0 1))
