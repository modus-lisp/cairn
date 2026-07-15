;;;; clone.lisp — fetching a repository over git's smart HTTP protocol.
;;;;
;;;; Two round-trips.  First a GET of info/refs advertises the remote's refs and
;;;; capabilities.  Then a POST names the commits we `want` and says `done`; the
;;;; server answers with a packfile of every object reachable from those wants.
;;;; We index that pack (index-pack.lisp) and lay down a .git — HEAD, the refs,
;;;; a config — and the repository is clonable by, and readable as, real git.

(in-package #:cairn)

(defun smart-http-base (url)
  "Normalise a clone URL to its smart-HTTP base (adding .git if missing)."
  (let ((u (string-right-trim "/" url)))
    (if (search ".git" u) u (concatenate 'string u ".git"))))

(defun auth-header (username token)
  "An HTTP Basic Authorization header, or NIL when USERNAME is unset."
  (when username
    (list (cons "Authorization"
                (concatenate 'string "Basic "
                             (base64-encode (string->bytes (format nil "~a:~a" username token))))))))

(defun discover-refs (base &key (service "git-upload-pack") auth)
  "GET BASE/info/refs?service=SERVICE.  Returns (values REFS CAPS HEAD-TARGET),
   REFS an alist of (refname . sha), CAPS the capability string, HEAD-TARGET the
   symref HEAD points at (or NIL).  AUTH is an optional Authorization header alist."
  (let* ((u (parse-url (format nil "~a/info/refs?service=~a" base service))))
    (multiple-value-bind (code hdrs body)
        (https-request "GET" (parsed-url-host u) (parsed-url-port u) (parsed-url-path u) :headers auth)
      (declare (ignore hdrs))
      (unless (= code 200) (error "cairn: info/refs returned HTTP ~d" code))
      (let ((pos 0) (refs '()) (caps "") (head-target nil) (first t))
        ;; leading "# service=..." comment pkt, then a flush
        (multiple-value-bind (p np kind) (read-pktline body pos)
          (declare (ignore p))
          (setf pos np)
          (when (eq kind :data)                          ; skip the trailing flush
            (multiple-value-setq (p np kind) (read-pktline body pos))
            (setf pos np)))
        (loop
          (multiple-value-bind (payload np kind) (read-pktline body pos)
            (setf pos np)
            (when (member kind '(:flush :response-end)) (return))
            (when (eq kind :data)
              (let* ((line (pktline-payload-string payload))
                     (nul (position #\Nul line)))
                (when (and first nul)                    ; caps ride the first ref line
                  (setf caps (subseq line (1+ nul)) line (subseq line 0 nul))
                  (let ((s (search "symref=HEAD:" caps)))
                    (when s
                      (let* ((start (+ s (length "symref=HEAD:")))
                             (end (position #\Space caps :start start)))
                        (setf head-target (subseq caps start end))))))
                (setf first nil)
                (let ((sp (position #\Space line)))
                  (push (cons (subseq line (1+ sp)) (subseq line 0 sp)) refs))))))
        (values (nreverse refs) caps head-target)))))

(defun fetch-pack (base wants &optional haves)
  "POST BASE/git-upload-pack with WANTS (SHA hex strings) — and, for an
   incremental fetch, HAVES we already hold — and receive the packfile bytes.
   With haves the server may return a thin pack."
  (let* ((u (parse-url (format nil "~a/git-upload-pack" base)))
         (body (let ((out (make-array 256 :element-type '(unsigned-byte 8)
                                           :adjustable t :fill-pointer 0)))
                 (loop for sha in wants for firstp = t then nil
                       for line = (if firstp
                                      (format nil "want ~a ofs-delta agent=cairn/0.1~%" sha)
                                      (format nil "want ~a~%" sha))
                       do (loop for b across (pktline line) do (vector-push-extend b out)))
                 (loop for b across +flush-pkt+ do (vector-push-extend b out))
                 (loop for sha in haves
                       do (loop for b across (pktline (format nil "have ~a~%" sha))
                                do (vector-push-extend b out)))
                 (loop for b across (pktline (format nil "done~%")) do (vector-push-extend b out))
                 (coerce out '(simple-array (unsigned-byte 8) (*))))))
    (multiple-value-bind (code hdrs resp)
        (https-request "POST" (parsed-url-host u) (parsed-url-port u) (parsed-url-path u)
                       :headers (list (cons "Content-Type" "application/x-git-upload-pack-request"))
                       :body body)
      (declare (ignore hdrs))
      (unless (= code 200) (error "cairn: git-upload-pack returned HTTP ~d" code))
      ;; response is one or more pkt-lines (NAK/ACK) then the raw packfile
      (let ((start (search #(#x50 #x41 #x43 #x4b) resp)))   ; "PACK"
        (unless start (error "cairn: no packfile in upload-pack response"))
        (subseq resp start)))))

(defun clone (url dest &key (checkout t))
  "Clone the remote git repository at URL (https) into directory DEST.  Fetches
   and indexes the packfile, writes a real .git, and (unless :checkout nil)
   materialises the working tree.  Returns the open repository."
  (let* ((base (smart-http-base url)))
    (multiple-value-bind (refs caps head-target) (discover-refs base)
      (declare (ignore caps))
      (let ((wants (refs->wants refs)))
        (format t "~&remote: ~d refs, fetching ~d wanted commits…~%" (length refs) (length wants))
        (let ((pack (fetch-pack base wants)))
          (format t "received packfile: ~d bytes~%" (length pack))
          (finish-clone dest url refs head-target pack checkout))))))

(defun peeled-ref-p (name)
  (and (> (length name) 3) (string= name "^{}" :start1 (- (length name) 3))))

(defun refs->wants (refs)
  "The distinct SHAs to ask for: every advertised ref except HEAD and peeled tags."
  (remove-duplicates
   (loop for (name . sha) in refs
         unless (or (string= name "HEAD") (peeled-ref-p name)) collect sha)
   :test #'string=))

(defun finish-clone (dest url refs head-target pack checkout)
  "Lay down a .git for DEST from advertised REFS + fetched PACK, index it, and
   (optionally) check out.  Shared by HTTP and SSH clone.  Returns the repo."
  (let* ((dest (uiop:ensure-directory-pathname dest))
         (git-dir (merge-pathnames ".git/" dest))
         (head-target (or head-target
                          (car (find-if (lambda (r) (search "refs/heads/" (car r))) refs))
                          "refs/heads/master")))
    (write-text-file (merge-pathnames "HEAD" git-dir) (format nil "ref: ~a~%" head-target))
    (write-text-file (merge-pathnames "config" git-dir)
                     (format nil "[core]~%	repositoryformatversion = 0~%	bare = false~%~
                                  [remote \"origin\"]~%	url = ~a~%" url))
    (loop for (name . sha) in refs
          unless (or (string= name "HEAD") (peeled-ref-p name))
            do (write-text-file (merge-pathnames name git-dir) (format nil "~a~%" sha)))
    (multiple-value-bind (pack-name count)
        (index-pack pack (merge-pathnames "objects/pack/" git-dir))
      (format t "indexed ~a: ~d objects~%" pack-name count))
    (let ((repo (open-repository dest)))
      (when checkout
        (format t "checked out ~d files~%" (checkout repo)))
      repo)))

;;; ---- push over smart HTTP (send-pack) ---------------------------------------

(defun remote-haves (repo refs)
  "The advertised remote-ref SHAs we already hold — the objects the remote has,
   so `send-pack` need not resend history reachable from any of its refs."
  (remove-duplicates
   (loop for (nil . sha) in refs
         when (and (not (string= sha +zero-sha+)) (have-object-p repo sha)) collect sha)
   :test #'string=))

(defun parse-report (buf)
  "Read a receive-pack report-status (pkt-lines in BUF) into a list of strings."
  (let ((pos 0) (lines '()))
    (loop
      (when (>= pos (length buf)) (return))
      (multiple-value-bind (payload np kind) (read-pktline buf pos)
        (setf pos np)
        (when (member kind '(:flush :response-end)) (return))
        (when (eq kind :data) (push (pktline-payload-string payload) lines))))
    (nreverse lines)))

(defun push-http (repo url &key username token ref)
  "Push REPO's current branch (or REF) to URL over smart HTTP.  USERNAME/TOKEN
   are HTTP Basic credentials (e.g. a GitHub username + personal-access-token).
   Returns the remote's report-status lines; signals if the update was rejected."
  (let* ((base (smart-http-base url))
         (auth (auth-header username token)))
    (multiple-value-bind (refs caps) (discover-refs base :service "git-receive-pack" :auth auth)
      (declare (ignore caps))
      (multiple-value-bind (kind localref) (head-ref repo)
        (let* ((refname (or ref (if (eq kind :symbolic) localref
                                    (error "cairn: detached HEAD, pass :ref"))))
               (new (head-commit repo))
               (old (or (cdr (assoc refname refs :test #'string=)) +zero-sha+))
               (send (objects-to-send repo new (remote-haves repo refs)))
               (pack (write-packfile repo send))
               (u (parse-url (format nil "~a/git-receive-pack" base)))
               (body (let ((out (byte-buffer)))
                       (push-bytes out (pktline (format nil "~a ~a ~a~creport-status agent=cairn/0.1~%"
                                                         old new refname #\Nul)))
                       (push-bytes out +flush-pkt+)
                       (push-bytes out pack)
                       (coerce out 'u8v))))
          (format t "~&pushing ~a  ~a -> ~a  (~d objects, ~d bytes)~%"
                  refname (short old) (short new) (length send) (length pack))
          (multiple-value-bind (code hdrs resp)
              (https-request "POST" (parsed-url-host u) (parsed-url-port u) (parsed-url-path u)
                             :headers (append (list (cons "Content-Type"
                                                          "application/x-git-receive-pack-request"))
                                              auth)
                             :body body)
            (declare (ignore hdrs))
            (unless (= code 200) (error "cairn: git-receive-pack returned HTTP ~d" code))
            (let ((report (parse-report resp)))
              (dolist (l report) (format t "  remote: ~a~%" l))
              (unless (find "unpack ok" report :test #'string=)
                (error "cairn: remote failed to unpack: ~a" report))
              (unless (some (lambda (l) (and (>= (length l) 3) (string= (subseq l 0 3) "ok "))) report)
                (error "cairn: ref update rejected: ~a" report))
              report)))))))
