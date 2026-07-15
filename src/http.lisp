;;;; http.lisp — a minimal HTTPS/1.1 client, just enough for git's smart HTTP.
;;;;
;;;; The smart HTTP transport is two requests: a GET of /info/refs to discover
;;;; the remote's refs, and a POST of /git-upload-pack carrying the want/have
;;;; negotiation and receiving the packfile.  We need no more of HTTP than GET,
;;;; POST, and reading a body delimited by Content-Length, chunked encoding, or
;;;; connection close.  TLS is `seal` (which is `natrium` underneath) — so a
;;;; clone travels cairn -> seal -> natrium, the whole ecosystem in one call.

(in-package #:cairn)

(defstruct parsed-url scheme host port path)

(defun parse-url (url)
  "Split URL into scheme/host/port/path.  https default port 443, http 80."
  (let* ((sep (search "://" url))
         (scheme (if sep (subseq url 0 sep) "https"))
         (rest (if sep (subseq url (+ sep 3)) url))
         (slash (position #\/ rest))
         (authority (if slash (subseq rest 0 slash) rest))
         (path (if slash (subseq rest slash) "/"))
         (colon (position #\: authority))
         (host (if colon (subseq authority 0 colon) authority))
         (port (if colon (parse-integer authority :start (1+ colon))
                   (if (string= scheme "http") 80 443))))
    (make-parsed-url :scheme scheme :host host :port port :path path)))

(defun crlf-line (stream)
  "Read one CRLF-terminated line from STREAM as a string (CRLF stripped).
   NIL at end of stream."
  (let ((out (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil :eof) do
      (cond ((eq b :eof) (return (when (plusp (length out)) (ascii out))))
            ((= b 13))                                   ; CR: swallow, expect LF next
            ((= b 10) (return (ascii out)))              ; LF: end of line
            (t (vector-push-extend b out))))))

(defun read-headers (stream)
  "Read HTTP headers until the blank line.  Returns an alist of (lc-name . value)."
  (loop for line = (crlf-line stream)
        while (and line (plusp (length line)))
        collect (let ((c (position #\: line)))
                  (cons (string-downcase (string-trim " " (subseq line 0 c)))
                        (string-trim " " (subseq line (1+ c)))))))

(defun read-n-bytes (stream n)
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (let ((got (read-sequence buf stream)))
      (when (< got n) (error "cairn: short HTTP body (~d of ~d bytes)" got n)))
    buf))

(defun read-chunked-body (stream)
  "Decode a Transfer-Encoding: chunked body into a single byte vector."
  (let ((out (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
      (let* ((line (crlf-line stream))
             (semi (and line (position #\; line)))
             (size (parse-integer line :end semi :radix 16)))
        (when (zerop size)
          (crlf-line stream)                              ; trailing CRLF after last chunk
          (return))
        (let ((chunk (read-n-bytes stream size)))
          (loop for b across chunk do (vector-push-extend b out)))
        (crlf-line stream)))                              ; CRLF after each chunk
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun read-to-eof (stream)
  (let ((out (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil :eof) until (eq b :eof)
          do (vector-push-extend b out))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun https-request (method host port path &key headers body)
  "Perform an HTTP request over TLS (via seal).  METHOD is \"GET\"/\"POST\";
   BODY is an optional byte vector.  Returns (values STATUS-CODE HEADERS-ALIST
   BODY-BYTES)."
  (let* ((conn (seal:connect host port))
         (stream (seal:make-tls-stream conn)))
    (unwind-protect
         (let* ((req-headers (append
                              (list (cons "Host" host)
                                    (cons "User-Agent" "git/cairn-0.1")
                                    (cons "Accept" "*/*")
                                    (cons "Connection" "close"))
                              headers
                              (when body
                                (list (cons "Content-Length"
                                            (princ-to-string (length body)))))))
                (head (with-output-to-string (s)
                        (format s "~a ~a HTTP/1.1~c~c" method path #\Return #\Linefeed)
                        (dolist (h req-headers)
                          (format s "~a: ~a~c~c" (car h) (cdr h) #\Return #\Linefeed))
                        (format s "~c~c" #\Return #\Linefeed))))
           (write-sequence (string->bytes head) stream)
           (when body (write-sequence body stream))
           (finish-output stream)
           ;; status line: "HTTP/1.1 200 OK"
           (let* ((status-line (crlf-line stream))
                  (sp (position #\Space status-line))
                  (code (parse-integer status-line :start (1+ sp) :end (+ sp 4)))
                  (hdrs (read-headers stream))
                  (te (cdr (assoc "transfer-encoding" hdrs :test #'string=)))
                  (cl (cdr (assoc "content-length" hdrs :test #'string=)))
                  (payload (cond ((and te (search "chunked" te)) (read-chunked-body stream))
                                 (cl (read-n-bytes stream (parse-integer cl)))
                                 (t (read-to-eof stream)))))
             (values code hdrs payload)))
      (ignore-errors (close stream)))))
