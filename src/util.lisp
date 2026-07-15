;;;; util.lisp — the primitives cairn borrows from the ecosystem.
;;;;
;;;; git's two workhorses are SHA-1 (content addressing) and DEFLATE (object
;;;; storage).  Neither is cairn's to reinvent: SHA-1 lives in `seal`, the
;;;; classical-crypto home (git relies on it as a hash, not for collision
;;;; resistance); DEFLATE is `chipz`, a pure-Common-Lisp inflate — no FFI, no
;;;; zlib.  Here we only adapt them to cairn's shapes (hex ids, a start offset
;;;; for packfile streams) and keep the small byte helpers.

(in-package #:cairn)

(deftype u8v () '(simple-array (unsigned-byte 8) (*)))

;;; --- SHA-1 (from seal) ---------------------------------------------------

(defun sha1 (msg)
  "SHA-1 of byte vector MSG → fresh 20-byte big-endian digest (via seal)."
  (seal:sha1 msg))

(defun bytes->hex (bytes)
  (string-downcase
   (with-output-to-string (s) (loop for b across bytes do (format s "~2,'0x" b)))))

(defun hex->bytes (hex)
  (let* ((n (/ (length hex) 2))
         (out (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n out)
      (setf (aref out i) (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun sha1-hex (msg) (bytes->hex (sha1 msg)))

;;; --- DEFLATE / zlib (from chipz) -----------------------------------------

(defun inflate (data &optional (start 0))
  "Raw DEFLATE (RFC 1951) decompress of DATA beginning at byte START."
  (chipz:decompress nil 'chipz:deflate data :input-start start))

(defun zlib-decompress (data &optional (start 0))
  "zlib (RFC 1950) decompress of DATA beginning at byte START.  Loose objects
   pass START 0; packfile objects pass the offset of their zlib stream — chipz
   stops at the stream's end, so no explicit length is needed."
  (chipz:decompress nil 'chipz:zlib data :input-start start))

;;; The compress side arrives with cairn's write path (salza2, chipz's sibling).
(defun deflate (bytes)
  (declare (ignore bytes)) (error "cairn: deflate (write side) not yet implemented"))
(defun zlib-compress (bytes)
  (declare (ignore bytes)) (error "cairn: zlib-compress (write side) not yet implemented"))
