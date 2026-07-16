;;;; serve.lisp — the whole sovereign git-over-SSH loop, on cairn + conch alone.
;;;;
;;;;   sbcl --non-interactive --load inspect/serve.lisp
;;;;
;;;; Runs cairn's git server (conch SSH + cairn's own upload-pack/receive-pack)
;;;; in a thread, then, from the same process, drives the full developer round
;;;; trip against it with cairn's client: CLONE a repo over SSH, edit a file,
;;;; COMMIT, and PUSH back — no C git, no OpenSSH anywhere in the path.  Real
;;;; `git fsck`/`git log` on the server repo afterwards is the only outside
;;;; witness: it confirms the pushed history is byte-valid git.

(require :asdf)
(require :sb-posix)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *dir* "/tmp/cairn-servetest")
(defvar *port* 2227)

(defun sh (&rest args) (uiop:run-program args :output :string :error-output :string))
(defun git (dir &rest args) (apply #'sh "git" "-C" dir args))

;;; ---- set up: a bare server repo with one commit, plus SSH keys -------------
(sh "rm" "-rf" *dir*)
(ensure-directories-exist (format nil "~a/" *dir*))
(let ((seed (format nil "~a/seed" *dir*)))
  (ensure-directories-exist (format nil "~a/" seed))
  (git seed "init" "-q" "-b" "master")
  (with-open-file (s (format nil "~a/README.md" seed) :direction :output)
    (write-string "# demo" s) (terpri s))
  (git seed "add" "README.md")
  (git seed "-c" "user.name=seed" "-c" "user.email=seed@localhost"
       "commit" "-q" "-m" "initial commit")
  (git seed "-c" "user.name=seed" "-c" "user.email=seed@localhost"
       "clone" "-q" "--bare" seed (format nil "~a/server.git" *dir*)))

(sh "ssh-keygen" "-t" "ed25519" "-f" (format nil "~a/host"   *dir*) "-N" "" "-q")
(sh "ssh-keygen" "-t" "ed25519" "-f" (format nil "~a/client" *dir*) "-N" "" "-q")

(defvar *server-repo* (format nil "~a/server.git" *dir*))
(defvar *host-key*    (format nil "~a/host"   *dir*))
(defvar *client-key*  (format nil "~a/client" *dir*))
(defvar *clone*       (format nil "~a/work"   *dir*))

;;; ---- the server: conch SSH + cairn git, in a thread ------------------------
(multiple-value-bind (csec cpub) (conch::load-private-key *client-key*)
  (declare (ignore csec))
  (let ((ready (sb-thread:make-semaphore)))
    (sb-thread:make-thread
     (lambda ()
       (handler-case
           (multiple-value-bind (hs hp) (conch::load-private-key *host-key*)
             (let ((listen (conch::tcp-listen *port*))
                   (handler (cairn:git-exec-handler)))
               (sb-thread:signal-semaphore ready)
               (loop
                 (conch:serve-connection (conch::accept-conn listen) hs hp
                                         :authorized-keys (list cpub)
                                         :handler handler))))
         (error (e) (format t "~&server thread: ~a~%" e))))
     :name "cairn-git-server")
    (sb-thread:wait-on-semaphore ready)

    ;; ---- client: clone over SSH -------------------------------------------
    (let ((url (format nil "ssh://tester@127.0.0.1:~d~a" *port* *server-repo*)))
      (format t "~&=== clone ===~%")
      (cairn:clone-ssh url *clone* :identity *client-key*)

      ;; ---- edit + commit on our side ------------------------------------
      (format t "~%=== edit + commit ===~%")
      (with-open-file (s (format nil "~a/README.md" *clone*)
                         :direction :output :if-exists :append)
        (write-line "a line added by the cairn client, committed and pushed over cairn-SSH." s))
      (with-open-file (s (format nil "~a/NEW.txt" *clone*) :direction :output)
        (write-line "a brand new file, never seen by the server before the push." s))
      (let ((repo (cairn:open-repository *clone*)))
        (cairn:add repo "README.md" "NEW.txt")
        (let ((sha (cairn:commit repo :message "client edit over sovereign SSH"
                                      :author "tester <tester@localhost>")))
          (format t "committed ~a~%" sha)

          ;; ---- push back over SSH ---------------------------------------
          (format t "~%=== push ===~%")
          (cairn:push-ssh repo url :identity *client-key*)

          ;; ---- verify with the outside witness: real git ---------------
          (format t "~%=== verify (real git on the server repo) ===~%")
          (let* ((branch (string-trim '(#\Newline #\Space)
                                      (git *server-repo* "symbolic-ref" "HEAD")))
                 (server-head (string-trim '(#\Newline #\Space)
                                           (git *server-repo* "rev-parse" branch)))
                 (fsck (nth-value 1 (git *server-repo* "fsck" "--strict")))
                 (blob (git *server-repo* "cat-file" "-p" (format nil "~a:NEW.txt" branch))))
            (format t "server ~a = ~a~%" branch server-head)
            (format t "git fsck: ~a~%" (if (string= "" (string-trim '(#\Newline) fsck)) "clean" fsck))
            (format t "server has NEW.txt: ~s~%" (string-trim '(#\Newline) blob))
            (format t "git log:~%~a" (git *server-repo* "log" "--oneline" branch))
            (let ((ok (and (string= server-head sha)
                           (search "brand new file" blob) t)))
              (format t "~%SOVEREIGN LOOP OK: ~a~%" ok)
              (sb-ext:exit :code (if ok 0 1)))))))))
