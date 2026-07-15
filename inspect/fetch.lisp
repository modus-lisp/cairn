;;;; fetch.lisp — incremental fetch + fast-forward pull over SSH.
;;;;
;;;; Needs the sshd from inspect/ssh.lisp running.  Builds a bare repo at C1,
;;;; clones it with cairn, has real git push a C2 (edit + add + delete), then
;;;; cairn fetches (only the new objects) and pulls (fast-forward + checkout),
;;;; and asks real git whether the result is clean.
;;;;
;;;;   sbcl --non-interactive --load inspect/fetch.lisp

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *bare* "/tmp/ftch-bare.git")
(defvar *dest* "/tmp/cairn-ft")
(defvar *url* "ssh://claude@127.0.0.1:2222/tmp/ftch-bare.git")
(defvar *id* "/tmp/sshtest/client_ed25519")

(flet ((sh (&rest args) (uiop:run-program args :output :string :error-output :string
                                          :ignore-error-status t)))
  ;; fresh bare repo at C1
  (dolist (d (list "/tmp/ftch-src" *bare* *dest* "/tmp/ftch-work"))
    (uiop:delete-directory-tree (uiop:ensure-directory-pathname d) :validate t :if-does-not-exist :ignore))
  (uiop:with-current-directory ((ensure-directories-exist "/tmp/ftch-src/"))
    (sh "git" "init" "-q" "-b" "main" ".")
    (with-open-file (s "/tmp/ftch-src/version.txt" :direction :output) (write-line "v1" s))
    (ensure-directories-exist "/tmp/ftch-src/doc/")
    (with-open-file (s "/tmp/ftch-src/doc/readme.md" :direction :output) (write-line "hello" s))
    (sh "git" "-C" "/tmp/ftch-src" "add" "-A")
    (sh "git" "-C" "/tmp/ftch-src" "-c" "user.name=seed" "-c" "user.email=s@s" "commit" "-qm" "C1"))
  (sh "git" "clone" "-q" "--bare" "/tmp/ftch-src" *bare*)

  ;; cairn clones C1
  (cairn:clone-ssh *url* *dest* :identity *id*)
  (let ((c1 (cairn:head-commit (cairn:open-repository *dest*))))

    ;; real git pushes C2
    (sh "git" "clone" "-q" *bare* "/tmp/ftch-work")
    (with-open-file (s "/tmp/ftch-work/version.txt" :direction :output :if-exists :supersede)
      (write-line "v2" s))
    (with-open-file (s "/tmp/ftch-work/feature.txt" :direction :output) (write-line "a new feature" s))
    (sh "git" "-C" "/tmp/ftch-work" "rm" "-q" "doc/readme.md")
    (sh "git" "-C" "/tmp/ftch-work" "add" "-A")
    (sh "git" "-C" "/tmp/ftch-work" "-c" "user.name=dev" "-c" "user.email=d@d" "commit" "-qm" "C2")
    (sh "git" "-C" "/tmp/ftch-work" "push" "-q" "origin" "main")

    ;; cairn fetch + pull
    (let ((repo (cairn:open-repository *dest*)))
      (format t "~&C1 = ~a~%" c1)
      (format t "-- fetch --~%")   (cairn:fetch repo :url *url* :identity *id*)
      (format t "local still ~a (fetch doesn't move HEAD)~%" (cairn:head-commit repo))
      (format t "-- pull --~%")    (cairn:pull repo :url *url* :identity *id*)
      (format t "local now  ~a~%" (cairn:head-commit repo))
      (format t "git status:  ~a~%"
              (if (string= "" (sh "git" "-C" *dest* "status" "-s")) "clean" "DIRTY"))
      (format t "git fsck:    ~a~%"
              (if (string= "" (sh "git" "-C" *dest* "fsck" "--full")) "clean" "errors"))
      (format t "HEAD == remote? ~a~%"
              (string= (string-trim '(#\Newline) (sh "git" "-C" *dest* "rev-parse" "HEAD"))
                       (string-trim '(#\Newline) (sh "git" "-C" *bare* "rev-parse" "HEAD")))))))
