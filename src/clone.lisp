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
  (let ()
    (multiple-value-bind (code hdrs body)
        (http-request "GET" (format nil "~a/info/refs?service=~a" base service) :headers auth)
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

;; The request body is the same for a pack fetched into memory and one streamed to disk.
(defun upload-pack-request (wants haves)
  "The git-upload-pack request body asking for WANTS, telling the server about HAVES."
  (let ((out (make-array 256 :element-type '(unsigned-byte 8)
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
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun fetch-pack (base wants &optional haves)
  "POST BASE/git-upload-pack with WANTS (SHA hex strings) — and, for an
   incremental fetch, HAVES we already hold — and receive the packfile bytes.
   With haves the server may return a thin pack.  The pack is returned in memory;
   FETCH-PACK-INTO streams it into a repository instead."
  (let* ((url (format nil "~a/git-upload-pack" base))
         (body (upload-pack-request wants haves)))
    (multiple-value-bind (code hdrs resp)
        (http-request "POST" url
                       :headers (list (cons "Content-Type" "application/x-git-upload-pack-request"))
                       :body body)
      (declare (ignore hdrs))
      (unless (= code 200) (error "cairn: git-upload-pack returned HTTP ~d" code))
      ;; response is one or more pkt-lines (NAK/ACK) then the raw packfile
      (let ((start (search #(#x50 #x41 #x43 #x4b) resp)))   ; "PACK"
        (unless start (error "cairn: no packfile in upload-pack response"))
        (subseq resp start)))))

(defun %read-fully (read buf start end)
  "Pull exactly BUF[START..END) through READ, or fewer only at the end of the body."
  (let ((i start))
    (loop while (< i end)
          do (let ((n (funcall read buf i end)))
               (when (zerop n) (return))
               (incf i n)))
    (- i start)))

(defun pack-chunks (read &key (buffer-size 65536))
  "Chunks of the pack in an upload-pack response read through READ: a function returning
   (values BUF END) for each piece and NIL at the end, as FS-WRITE-STREAM takes them.  The
   pkt-lines before the pack (NAK / ACK) are read and dropped.  BUF is reused."
  (let ((buf (make-array buffer-size :element-type '(unsigned-byte 8)))
        (started nil))
    (lambda ()
      (if started
          (let ((n (funcall read buf 0 (length buf))))
            (and (plusp n) (values buf n)))
          (loop
            (unless (= 4 (%read-fully read buf 0 4))
              (error "cairn: no packfile in upload-pack response"))
            (when (equalp (subseq buf 0 4) #(#x50 #x41 #x43 #x4b))  ; "PACK"
              (setf started t)
              (return (values buf (+ 4 (funcall read buf 4 (length buf))))))
            (let ((len (or (ignore-errors (parse-integer (map 'string #'code-char (subseq buf 0 4))
                                                         :radix 16))
                           (error "cairn: bad pkt-line before the pack"))))
              (when (> len 4)                                       ; skip its payload
                (let ((skip (make-array (- len 4) :element-type '(unsigned-byte 8))))
                  (unless (= (- len 4) (%read-fully read skip 0 (- len 4)))
                    (error "cairn: upload-pack response cut off"))))))))))

(defun fetch-pack-into (repo base wants &optional haves)
  "Fetch the pack for WANTS (and HAVES) from BASE and stream it straight into REPO's store,
   then index it there: the pack is never held in memory.  Returns (values PACK-NAME
   OBJECT-COUNT OCTETS)."
  (multiple-value-bind (code hdrs read close)
      (http-open-body "POST" (format nil "~a/git-upload-pack" base)
                      :headers (list (cons "Content-Type" "application/x-git-upload-pack-request"))
                      :body (upload-pack-request wants haves))
    (declare (ignore hdrs))
    (unwind-protect
         (progn
           (unless (= code 200) (error "cairn: git-upload-pack returned HTTP ~d" code))
           (receive-pack-stream repo (pack-chunks read)))
      (funcall close))))

(defun clone (url dest &key (checkout t))
  "Clone the remote git repository at URL (https) into directory DEST.  Fetches
   and indexes the packfile, writes a real .git, and (unless :checkout nil)
   materialises the working tree.  Returns the open repository."
  (let* ((base (smart-http-base url)))
    (multiple-value-bind (refs caps head-target) (discover-refs base)
      (declare (ignore caps))
      (let ((wants (refs->wants refs)))
        (format t "~&remote: ~d refs, fetching ~d wanted commits…~%" (length refs) (length wants))
        (let ((repo (start-clone dest url refs head-target)))
          (multiple-value-bind (pack-name count octets) (fetch-pack-into repo base wants)
            (format t "received packfile: ~d bytes~%indexed ~a: ~d objects~%" octets pack-name count))
          (when checkout
            (format t "checked out ~d files~%" (checkout repo)))
          repo)))))

(defun peeled-ref-p (name)
  (and (> (length name) 3) (string= name "^{}" :start1 (- (length name) 3))))

(defun refs->wants (refs)
  "The distinct SHAs to ask for: every advertised ref except HEAD and peeled tags."
  (remove-duplicates
   (loop for (name . sha) in refs
         unless (or (string= name "HEAD") (peeled-ref-p name)) collect sha)
   :test #'string=))

(defun start-clone (dest url refs head-target)
  "Lay down a .git for DEST from advertised REFS -- HEAD, config, refs, no objects yet --
   and return the repository."
  (let* ((dest (uiop:ensure-directory-pathname dest))
         (git-dir (merge-pathnames ".git/" dest))
         (repo (let ((r (make-repository-on-backend (make-host-backend git-dir)
                                                    :path dest :git-dir git-dir)))
                 (setf (repo-worktree r) (make-host-worktree-backend dest)) r))
         (head-target (or head-target
                          (car (find-if (lambda (r) (search "refs/heads/" (car r))) refs))
                          "refs/heads/master")))
    (fs-write-string repo "HEAD" (format nil "ref: ~a~%" head-target))
    (fs-write-string repo "config"
                     (format nil "[core]~%	repositoryformatversion = 0~%	bare = false~%~
                                  [remote \"origin\"]~%	url = ~a~%" url))
    (loop for (name . sha) in refs
          unless (or (string= name "HEAD") (peeled-ref-p name))
            do (fs-write-string repo name (format nil "~a~%" sha)))
    repo))

(defun finish-clone (dest url refs head-target pack checkout)
  "Lay down a .git for DEST from advertised REFS + fetched PACK (in memory), index it, and
   (optionally) check out.  Used by SSH clone.  Returns the repo."
  (let ((repo (start-clone dest url refs head-target)))
    (multiple-value-bind (pack-name count) (index-pack repo pack)
      (format t "indexed ~a: ~d objects~%" pack-name count))
    (when checkout
      (format t "checked out ~d files~%" (checkout repo)))
    repo))

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
               (url (format nil "~a/git-receive-pack" base))
               (body (let ((out (byte-buffer)))
                       (push-bytes out (pktline (format nil "~a ~a ~a~creport-status agent=cairn/0.1~%"
                                                         old new refname #\Nul)))
                       (push-bytes out +flush-pkt+)
                       (push-bytes out pack)
                       (coerce out 'u8v))))
          (format t "~&pushing ~a  ~a -> ~a  (~d objects, ~d bytes)~%"
                  refname (short old) (short new) (length send) (length pack))
          (multiple-value-bind (code hdrs resp)
              (http-request "POST" url
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
