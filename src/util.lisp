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

;;; --- CRC-32 (IEEE, for packfile .idx v2) ---------------------------------

(defparameter *crc32-table*
  (let ((tbl (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 tbl)
      (let ((c n))
        (dotimes (_ 8)
          (setf c (if (logtest c 1)
                      (logxor #xedb88320 (ash c -1))
                      (ash c -1))))
        (setf (aref tbl n) c))))
  "CRC-32 lookup table (reflected polynomial #xEDB88320).")

(defun crc32 (bytes &optional (start 0) (end (length bytes)))
  "IEEE CRC-32 of BYTES[START:END] — the checksum git records per object in a
   packfile index."
  (let ((c #xffffffff))
    (loop for i from start below end do
      (setf c (logxor (ash c -8)
                      (aref *crc32-table* (logand (logxor c (aref bytes i)) #xff)))))
    (logxor c #xffffffff)))

;;; The compress side is salza2 — chipz's pure-Common-Lisp sibling.
(defun deflate (bytes)
  "Raw DEFLATE (RFC 1951) compress of BYTES."
  (salza2:compress-data (coerce bytes 'u8v) 'salza2:deflate-compressor))

(defun zlib-compress (bytes)
  "zlib (RFC 1950) compress of BYTES — how git stores a loose object."
  (salza2:compress-data (coerce bytes 'u8v) 'salza2:zlib-compressor))

;;; --- byte-buffer + file helpers (shared by the pack/index writers) -----------

(defun %push-be16 (vec u)
  (vector-push-extend (logand (ash u -8) #xff) vec)
  (vector-push-extend (logand u #xff) vec))

(defun %push-be32 (vec u)
  (vector-push-extend (logand (ash u -24) #xff) vec)
  (vector-push-extend (logand (ash u -16) #xff) vec)
  (vector-push-extend (logand (ash u -8) #xff) vec)
  (vector-push-extend (logand u #xff) vec))

(defun %push-be64 (vec u)
  (%push-be32 vec (logand (ash u -32) #xffffffff))
  (%push-be32 vec (logand u #xffffffff)))

(defun byte-buffer ()
  (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun push-bytes (vec bytes)
  (loop for b across bytes do (vector-push-extend b vec)))

(defun write-bytes (path bytes)
  (ensure-directories-exist path)
  (with-open-file (s path :element-type '(unsigned-byte 8)
                          :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-sequence bytes s))
  path)

(defun write-text-file (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create :external-format :utf-8)
    (write-string text s))
  path)
