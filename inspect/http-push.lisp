;;;; http-push.lisp — push over smart HTTP to a real remote (opt-in).
;;;;
;;;; Pushes a throwaway commit to a test branch over HTTPS with Basic auth, then
;;;; you delete the branch.  Because it mutates a real remote it is opt-in:
;;;;
;;;;   CAIRN_PUSH_URL=https://github.com/you/repo \
;;;;   CAIRN_PUSH_USER=you CAIRN_PUSH_TOKEN=$(gh auth token) \
;;;;   sbcl --non-interactive --load inspect/http-push.lisp
;;;;
;;;; Then clean up:
;;;;   gh api -X DELETE repos/you/repo/git/refs/heads/cairn-http-test

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(let ((url   (sb-ext:posix-getenv "CAIRN_PUSH_URL"))
      (user  (sb-ext:posix-getenv "CAIRN_PUSH_USER"))
      (token (sb-ext:posix-getenv "CAIRN_PUSH_TOKEN")))
  (if (not (and url user token))
      (format t "~&set CAIRN_PUSH_URL / CAIRN_PUSH_USER / CAIRN_PUSH_TOKEN to run~%")
      (let ((dest "/tmp/cairn-httppush"))
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname dest)
                                    :validate t :if-does-not-exist :ignore)
        (uiop:run-program (list "git" "clone" "-q" url dest))
        (uiop:run-program (list "git" "-C" dest "checkout" "-q" "-b" "cairn-http-test"))
        (uiop:with-output-file (s (format nil "~a/CAIRN-HTTP-PUSH.txt" dest) :if-exists :supersede)
          (write-line "pushed over cairn HTTP+TLS; delete me" s))
        (uiop:run-program (list "git" "-C" dest "add" "-A"))
        (uiop:run-program (list "git" "-C" dest "-c" "user.name=cairn" "-c" "user.email=c@c"
                                "commit" "-qm" "test: cairn HTTP push"))
        (let ((repo (cairn:open-repository dest)))
          (format t "~&pushing ~a to ~a over HTTP…~%" (subseq (cairn:head-commit repo) 0 8) url)
          (cairn:push-http repo url :username user :token token :ref "refs/heads/cairn-http-test")
          (format t "~&pushed. delete the branch when done (see header).~%")))))
