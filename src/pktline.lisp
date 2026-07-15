;;;; pktline.lisp — the pkt-line framing of git's wire protocols.
;;;;
;;;; Every git transport (over HTTP, SSH, or git://) speaks in *pkt-lines*: a
;;;; 4-byte hex length prefix (counting itself) followed by that many bytes of
;;;; payload.  Three lengths are special control frames: 0000 = flush,
;;;; 0001 = delim, 0002 = response-end.  We read from a byte buffer with an
;;;; explicit cursor (HTTP hands us the whole body at once) and build request
;;;; bodies as byte vectors.

(in-package #:cairn)

(defun read-pktline (buf pos)
  "Read one pkt-line from BUF at POS.  Returns (values PAYLOAD NEW-POS KIND),
   where KIND is :data (PAYLOAD = a byte subseq) or :flush/:delim/:response-end
   (PAYLOAD = NIL).  Signals on a truncated frame."
  (when (> (+ pos 4) (length buf))
    (error "cairn: truncated pkt-line length at ~d" pos))
  (let ((len (parse-integer (ascii (subseq buf pos (+ pos 4))) :radix 16)))
    (cond
      ((= len 0) (values nil (+ pos 4) :flush))
      ((= len 1) (values nil (+ pos 4) :delim))
      ((= len 2) (values nil (+ pos 4) :response-end))
      ((< len 4) (error "cairn: invalid pkt-line length ~d" len))
      (t (let ((end (+ pos len)))
           (when (> end (length buf))
             (error "cairn: pkt-line runs past buffer (len ~d at ~d)" len pos))
           (values (subseq buf (+ pos 4) end) end :data))))))

(defun pktline (payload)
  "Encode PAYLOAD (a string or byte vector) as one pkt-line byte vector."
  (let* ((bytes (if (stringp payload) (string->bytes payload) payload))
         (len (+ 4 (length bytes)))
         (hdr (string->bytes (format nil "~4,'0x" len))))
    (concatenate '(simple-array (unsigned-byte 8) (*)) hdr bytes)))

(defparameter +flush-pkt+ (string->bytes "0000")
  "The flush-pkt that terminates a pkt-line stream / section.")

(defun pktline-payload-string (payload)
  "PAYLOAD bytes of a data pkt-line as a string, trailing newline trimmed."
  (string-right-trim '(#\Newline) (ascii payload)))
