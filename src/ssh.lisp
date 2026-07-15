;;;; ssh.lisp — git over SSH, riding on conch.
;;;;
;;;; The same pack protocol as smart HTTP, but the transport is an SSH exec
;;;; channel instead of two HTTP requests.  We `conch:connect`, open a session,
;;;; and exec `git-upload-pack '<path>'` (to fetch) or `git-receive-pack
;;;; '<path>'` (to push); then it's pkt-lines and a packfile over the channel.
;;;; This is where the whole stack stands up at once: cairn's git protocol over
;;;; conch's SSH over natrium's crypto, no OpenSSH, no libgit2, no shelling out.

(in-package #:cairn)

(defstruct (ssh-target (:conc-name st-)) user host port path)

(defun parse-ssh-url (url)
  "Parse ssh://[user@]host[:port]/path or the scp-style user@host:path."
  (if (and (>= (length url) 6) (string= (subseq url 0 6) "ssh://"))
      (let* ((rest (subseq url 6))
             (slash (position #\/ rest))
             (authority (subseq rest 0 slash))
             (path (subseq rest slash))
             (at (position #\@ authority))
             (user (and at (subseq authority 0 at)))
             (hp (if at (subseq authority (1+ at)) authority))
             (colon (position #\: hp)))
        (make-ssh-target :user user :host (if colon (subseq hp 0 colon) hp)
                         :port (if colon (parse-integer hp :start (1+ colon)) 22) :path path))
      (let* ((at (position #\@ url))
             (user (and at (subseq url 0 at)))
             (rest (if at (subseq url (1+ at)) url))
             (colon (position #\: rest)))
        (make-ssh-target :user user :host (subseq rest 0 colon) :port 22
                         :path (subseq rest (1+ colon))))))

(defun chan-read-pktline (c)
  "Read one pkt-line from an SSH channel: (values PAYLOAD KIND)."
  (let ((len (parse-integer (ascii (conch:chan-read-exact c 4)) :radix 16)))
    (cond ((= len 0) (values nil :flush))
          ((<= len 3) (values nil :special))
          (t (values (conch:chan-read-exact c (- len 4)) :data)))))

(defun read-ref-advert (c)
  "Read a ref advertisement from channel C: (values REFS CAPS HEAD-TARGET)."
  (let ((refs '()) (caps "") (head-target nil) (first t))
    (loop
      (multiple-value-bind (payload kind) (chan-read-pktline c)
        (when (member kind '(:flush :special)) (return))
        (let* ((line (pktline-payload-string payload))
               (nul (position #\Nul line)))
          (when (and first nul)
            (setf caps (subseq line (1+ nul)) line (subseq line 0 nul))
            (let ((s (search "symref=HEAD:" caps)))
              (when s
                (let ((start (+ s (length "symref=HEAD:"))))
                  (setf head-target (subseq caps start (position #\Space caps :start start)))))))
          (setf first nil)
          (let ((sp (position #\Space line)))
            (when (and sp (not (string= (subseq line (1+ sp)) "capabilities^{}")))
              (push (cons (subseq line (1+ sp)) (subseq line 0 sp)) refs))))))
    (values (nreverse refs) caps head-target)))

(defun ssh-git-channel (target service identity)
  "Connect to TARGET and exec SERVICE (git-upload-pack/git-receive-pack) on its
   path.  Returns (values CONN CHAN)."
  (let* ((conn (conch:connect (st-host target) :port (st-port target)
                              :user (or (st-user target) (sb-ext:posix-getenv "USER") "git")
                              :identity identity))
         (chan (conch:open-session conn)))
    (conch:chan-exec chan (format nil "~a '~a'" service (st-path target)))
    (values conn chan)))

;;; ---- fetch / clone ----------------------------------------------------------

(defun send-wants (chan wants &optional haves)
  (loop for sha in wants for firstp = t then nil
        do (conch:chan-write chan (pktline (if firstp
                                               (format nil "want ~a ofs-delta agent=cairn/0.1~%" sha)
                                               (format nil "want ~a~%" sha)))))
  (conch:chan-write chan +flush-pkt+)
  (loop for sha in haves do (conch:chan-write chan (pktline (format nil "have ~a~%" sha))))
  (conch:chan-write chan (pktline (format nil "done~%"))))

(defun clone-ssh (url dest &key identity (checkout t))
  "Clone URL (ssh://… or user@host:path) into DEST over SSH."
  (let ((target (parse-ssh-url url)))
    (multiple-value-bind (conn chan) (ssh-git-channel target "git-upload-pack" identity)
      (unwind-protect
           (multiple-value-bind (refs caps head-target) (read-ref-advert chan)
             (declare (ignore caps))
             (let ((wants (refs->wants refs)))
               (format t "~&remote: ~d refs, fetching ~d wanted commits…~%" (length refs) (length wants))
               (send-wants chan wants)
               (let* ((resp (conch:chan-read-all chan))
                      (start (search #(#x50 #x41 #x43 #x4b) resp)))
                 (unless start (error "cairn: no packfile in upload-pack response"))
                 (format t "received packfile: ~d bytes~%" (- (length resp) start))
                 (finish-clone dest url refs head-target (subseq resp start) checkout))))
        (ignore-errors (conch:chan-close chan))
        (ignore-errors (conch:disconnect conn))))))

;;; ---- push (send-pack) -------------------------------------------------------

(defparameter +zero-sha+ "0000000000000000000000000000000000000000")
(defun short (sha) (subseq sha 0 (min 8 (length sha))))

(defun read-report (c)
  "Read a receive-pack report-status: unpack line + per-ref status until flush."
  (let ((lines '()))
    (loop
      (multiple-value-bind (payload kind) (chan-read-pktline c)
        (when (member kind '(:flush :special)) (return))
        (push (pktline-payload-string payload) lines)))
    (nreverse lines)))

(defun push-ssh (repo url &key identity ref)
  "Push REPO's current branch (or REF) to URL over SSH.  Returns the remote's
   report-status lines; signals if the ref update was rejected."
  (let ((target (parse-ssh-url url)))
    (multiple-value-bind (conn chan) (ssh-git-channel target "git-receive-pack" identity)
      (unwind-protect
           (multiple-value-bind (refs caps) (read-ref-advert chan)
             (declare (ignore caps))
             (multiple-value-bind (kind localref) (head-ref repo)
               (let* ((refname (or ref (if (eq kind :symbolic) localref
                                           (error "cairn: detached HEAD, pass :ref"))))
                      (new (head-commit repo))
                      (old (or (cdr (assoc refname refs :test #'string=)) +zero-sha+))
                      (haves (unless (string= old +zero-sha+) (list old)))
                      (send (objects-to-send repo new haves))
                      (pack (write-packfile repo send)))
                 (format t "~&pushing ~a  ~a -> ~a  (~d objects, ~d bytes)~%"
                         refname (short old) (short new) (length send) (length pack))
                 (conch:chan-write chan
                   (pktline (format nil "~a ~a ~a~creport-status agent=cairn/0.1~%"
                                    old new refname #\Nul)))
                 (conch:chan-write chan +flush-pkt+)
                 (conch:chan-write chan pack)
                 (conch:chan-send-eof chan)
                 (let ((report (read-report chan)))
                   (dolist (l report) (format t "  remote: ~a~%" l))
                   (unless (find "unpack ok" report :test #'string=)
                     (error "cairn: remote failed to unpack: ~a" report))
                   (unless (some (lambda (l) (and (>= (length l) 3) (string= (subseq l 0 3) "ok "))) report)
                     (error "cairn: ref update rejected: ~a" report))
                   report))))
        (ignore-errors (conch:chan-close chan))
        (ignore-errors (conch:disconnect conn))))))
