;;;; serve.lisp — the *server* side of git-over-SSH (upload-pack / receive-pack).
;;;;
;;;; cairn is a git client that speaks the smart transfer protocol to a server;
;;;; this is the mirror — cairn *being* the server, over a conch SSH channel.  A
;;;; git client that runs `git-upload-pack '<repo>'` (to clone/fetch) or
;;;; `git-receive-pack '<repo>'` (to push) gets served entirely by cairn: we
;;;; advertise the refs, read the client's wants/haves or update-commands + pack,
;;;; and answer — build a pack to send, or index a pushed pack and move the refs.
;;;; Wire git-exec-handler into conch:serve and you have a self-hosted git forge
;;;; with no C git and no OpenSSH anywhere in the path.

(in-package #:cairn)

(defun head-symref-target (repo)
  (multiple-value-bind (kind ref) (head-ref repo)
    (when (eq kind :symbolic) ref)))

(defun advertise-refs (repo chan)
  "Send the ref advertisement (git-upload/receive-pack format) over the SSH CHAN:
   the refs as pkt-lines — the first carrying capabilities and the symref HEAD —
   terminated by a flush."
  (let* ((refs (sort (remove-if (lambda (r) (peeled-ref-p (car r))) (list-refs repo))
                     #'string< :key #'car))
         (head (ignore-errors (head-commit repo)))
         (target (head-symref-target repo))
         (caps (format nil "ofs-delta agent=cairn/0.1~@[ symref=HEAD:~a~]" target))
         (first t))
    (flet ((line (s) (conch:chan-write chan (pktline s))))
      (when head (line (format nil "~a HEAD~c~a~%" head #\Nul caps)) (setf first nil))
      (dolist (r refs)
        (if first
            (progn (line (format nil "~a ~a~c~a~%" (cdr r) (car r) #\Nul caps)) (setf first nil))
            (line (format nil "~a ~a~%" (cdr r) (car r)))))
      (when first                                        ; empty repo
        (line (format nil "~a capabilities^{}~c~a~%" +zero-sha+ #\Nul caps))))
    (conch:chan-write chan +flush-pkt+)))

(defun pkt-token (line prefix)
  "If LINE begins with PREFIX, the token after it (up to a space or end)."
  (when (and (>= (length line) (length prefix)) (string= line prefix :end1 (length prefix)))
    (let* ((start (length prefix)) (sp (position #\Space line :start start)))
      (subseq line start (or sp (length line))))))

(defun serve-upload-pack (repo chan)
  "The git-upload-pack side (serves clone/fetch): advertise refs, read the
   client's wants/haves/done, then send NAK and a packfile of what was asked for."
  (advertise-refs repo chan)
  (let ((wants '()) (haves '()) (done nil))
    (loop until done do
      (multiple-value-bind (payload kind) (chan-read-pktline chan)
        (ecase kind
          (:flush)                                       ; wants/haves separator
          (:special (setf done t))
          (:data (let ((line (pktline-payload-string payload)))
                   (cond ((pkt-token line "want ") (push (pkt-token line "want ") wants))
                         ((pkt-token line "have ") (push (pkt-token line "have ") haves))
                         ((string= line "done") (setf done t))))))))
    (let ((have (make-hash-table :test 'equal))
          (want (make-hash-table :test 'equal)) (send '()))
      (dolist (h haves) (when (have-object-p repo h) (reachable-objects repo h have)))
      (dolist (w wants) (reachable-objects repo w want))
      (maphash (lambda (sha ty) (declare (ignore ty))
                 (unless (gethash sha have) (push sha send)))
               want)
      (conch:chan-write chan (pktline (format nil "NAK~%")))
      (conch:chan-write chan (write-packfile repo send)))))

(defun serve-receive-pack (repo chan)
  "The git-receive-pack side (serves push): advertise refs, read the client's
   ref-update commands and packfile, index the pack, move the refs, and report."
  (advertise-refs repo chan)
  (let ((commands '()))
    (loop
      (multiple-value-bind (payload kind) (chan-read-pktline chan)
        (when (member kind '(:flush :special)) (return))
        (when (eq kind :data)
          (let* ((line (pktline-payload-string payload))
                 (nul (position #\Nul line))
                 (line (if nul (subseq line 0 nul) line))
                 (sp1 (position #\Space line))
                 (sp2 (position #\Space line :start (1+ sp1))))
            (push (list (subseq line 0 sp1) (subseq line (1+ sp1) sp2) (subseq line (1+ sp2)))
                  commands)))))
    ;; the packfile follows (the client half-closes its side after sending it)
    (let* ((pack (conch:chan-read-all chan))
           (start (and pack (search #(#x50 #x41 #x43 #x4b) pack))))
      (when start
        (index-pack (subseq pack start)
                    (merge-pathnames "objects/pack/" (repo-git-dir repo)) repo)
        (setf (repo-packs repo) :unloaded)))
    (let ((report (byte-buffer)))
      (push-bytes report (pktline (format nil "unpack ok~%")))
      (dolist (cmd (nreverse commands))
        (destructuring-bind (old new ref) cmd
          (declare (ignore old))
          (if (string= new +zero-sha+)
              (uiop:delete-file-if-exists (merge-pathnames ref (repo-git-dir repo)))
              (update-ref repo ref new))
          (push-bytes report (pktline (format nil "ok ~a~%" ref)))))
      (push-bytes report +flush-pkt+)
      (conch:chan-write chan (coerce report 'u8v)))))

(defun parse-git-command (command)
  "(values SERVICE PATH) from e.g. \"git-upload-pack '/srv/repo.git'\"."
  (let* ((sp (position #\Space command))
         (service (subseq command 0 sp))
         (path (string-trim "'\" " (subseq command (1+ sp)))))
    (values service path)))

(defun git-exec-handler ()
  "A conch:serve HANDLER (command chan -> exit-code) that serves git-over-SSH:
   dispatches git-upload-pack / git-receive-pack to cairn on the requested repo."
  (lambda (command chan)
    (handler-case
        (multiple-value-bind (service path) (parse-git-command command)
          (let ((repo (open-repository path)))
            (cond ((string= service "git-upload-pack")  (serve-upload-pack repo chan) 0)
                  ((string= service "git-receive-pack") (serve-receive-pack repo chan) 0)
                  (t 1))))
      (error () 1))))

(defun serve-git (port &key host-key authorized-keys)
  "Run a git-over-SSH server on PORT — conch's SSH server + cairn's own upload-
   pack/receive-pack.  The repository is the path in the client's ssh command.
   HOST-KEY is the server's Ed25519 host key; AUTHORIZED-KEYS the allowed keys."
  (conch:serve port :host-key host-key :authorized-keys authorized-keys
                    :handler (git-exec-handler)))
