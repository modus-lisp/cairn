;;;; ssh.lisp — git over SSH end to end: clone, commit, push, ask real git.
;;;;
;;;; Needs a local sshd + a bare repo to serve.  Set up (once):
;;;;
;;;;   D=/tmp/sshtest; mkdir -p $D
;;;;   ssh-keygen -t ed25519 -f $D/host_ed25519   -N "" -q
;;;;   ssh-keygen -t ed25519 -f $D/client_ed25519 -N "" -q
;;;;   cp $D/client_ed25519.pub $D/authorized_keys; chmod 600 $D/authorized_keys
;;;;   cat > $D/sshd_config <<EOF
;;;;   Port 2222
;;;;   ListenAddress 127.0.0.1
;;;;   HostKey $D/host_ed25519
;;;;   AuthorizedKeysFile $D/authorized_keys
;;;;   PidFile $D/sshd.pid
;;;;   PasswordAuthentication no
;;;;   StrictModes no
;;;;   EOF
;;;;   /usr/sbin/sshd -f $D/sshd_config -D -e &
;;;;   # a bare repo to serve:
;;;;   S=/tmp/ssh-src; git init -q $S; (cd $S; echo hi > f; git add -A; \
;;;;     git -c user.name=t -c user.email=t@t commit -qm init)
;;;;   git clone -q --bare $S /tmp/ssh-bare.git
;;;;
;;;; Then: sbcl --non-interactive --load inspect/ssh.lisp

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *url* "ssh://claude@127.0.0.1:2222/tmp/ssh-bare.git")
(defvar *id*  "/tmp/sshtest/client_ed25519")
(defvar *dest* "/tmp/cairn-ssh")

(uiop:delete-directory-tree (uiop:ensure-directory-pathname *dest*)
                            :validate t :if-does-not-exist :ignore)

(let ((repo (cairn:clone-ssh *url* *dest* :identity *id*)))
  (format t "~&cloned over SSH, HEAD ~a~%" (cairn:head-commit repo))
  (with-open-file (s (format nil "~a/pushed.txt" *dest*) :direction :output :if-exists :supersede)
    (write-line "added and pushed by cairn over ssh" s))
  (cairn:add repo "pushed.txt")
  (let ((sha (cairn:commit repo :message "cairn: pushed over ssh"
                                :author "ynniv <anthropic@ynniv.com>")))
    (cairn:push-ssh repo *url* :identity *id*)
    (let ((remote (string-trim '(#\Newline)
                    (uiop:run-program (list "git" "-C" "/tmp/ssh-bare.git" "rev-parse" "HEAD")
                                      :output :string :ignore-error-status t))))
      (format t "~%local commit:  ~a~%remote HEAD:   ~a  [match: ~a]~%"
              sha remote (string= sha remote))
      (format t "remote fsck:   ~a~%"
              (let ((o (uiop:run-program (list "git" "-C" "/tmp/ssh-bare.git" "fsck" "--full")
                                         :output :string :error-output :string :ignore-error-status t)))
                (if (string= o "") "clean" o))))))
