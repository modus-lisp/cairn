;;;; util.lisp — the primitives cairn borrows from the ecosystem.
;;;;
;;;; git's two workhorses are SHA-1 (content addressing) and DEFLATE (object
;;;; storage).  Neither is cairn's to reinvent: SHA-1 lives in `seal`, the
;;;; classical-crypto home (git relies on it as a hash, not for collision
;;;; resistance); DEFLATE is `cram`, a pure-Common-Lisp zlib codec — no FFI, no
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

;;; --- object id, hash-agnostic (SHA-1 or SHA-256) -----------------------------
;;;
;;; git can address objects by SHA-256 instead of SHA-1 (repositories created
;;; with --object-format=sha256).  Everything downstream — object ids, tree
;;; entry widths, pack/index layouts — follows from the digest, so we bind *OID*
;;; to the repository's format and read the width and hash function from it.

(defvar *oid* :sha1 "The active object format: :sha1 or :sha256.")

(defun oid-digest (bytes)
  (ecase *oid* (:sha1 (seal:sha1 bytes)) (:sha256 (seal:sha256 bytes))))
(defun oid-nbytes () (ecase *oid* (:sha1 20) (:sha256 32)))
(defun oid-nhex   () (ecase *oid* (:sha1 40) (:sha256 64)))
(defun oid-hex (bytes) (bytes->hex (oid-digest bytes)))

(defun oid-hex-parts (&rest parts)
  "Hex object id of the concatenation of PARTS (byte vectors) under *oid* —
   streamed for SHA-1, so the parts (git's header + content) are never copied
   into one buffer.  This is the object-hashing hot path during a clone."
  (ecase *oid*
    (:sha1 (let ((s (seal:sha1-init)))
             (dolist (p parts) (seal:sha1-update s p))
             (bytes->hex (seal:sha1-final s))))
    (:sha256 (bytes->hex (seal:sha256 (apply #'concatenate 'u8v parts))))))

;;; --- DEFLATE / zlib (from cram) ------------------------------------------

(defun inflate (data &optional (start 0))
  "Raw DEFLATE (RFC 1951) decompress of DATA beginning at byte START."
  (values (cram:deflate-decompress data :start start)))

(defun zlib-decompress (data &optional (start 0))
  "zlib (RFC 1950) decompress of DATA beginning at byte START.  Loose objects
   pass START 0; packfile objects pass the offset of their zlib stream — cram
   stops at the stream's end, so no explicit length is needed."
  (values (cram:zlib-decompress data :start start)))

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

;;; The compress side is cram too (fixed-Huffman + LZ77; standard zlib output).
(defun deflate (bytes)
  "Raw DEFLATE (RFC 1951) compress of BYTES."
  (cram:deflate-compress (coerce bytes 'u8v)))

(defun zlib-compress (bytes)
  "zlib (RFC 1950) compress of BYTES — how git stores a loose object."
  (cram:zlib-compress (coerce bytes 'u8v)))

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

(defparameter +zero-sha+ "0000000000000000000000000000000000000000"
  "The all-zero object id — a ref's \"old\" value when it is being created.")

(defun short (sha) (subseq sha 0 (min 8 (length sha))))

(defun base64-encode (bytes)
  "Standard base64 of a byte vector (for HTTP Basic auth)."
  (let ((tbl "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (out (make-string-output-stream)) (n (length bytes)))
    (loop for i from 0 below n by 3 do
      (let ((triple (logior (ash (aref bytes i) 16)
                            (ash (if (< (+ i 1) n) (aref bytes (+ i 1)) 0) 8)
                            (if (< (+ i 2) n) (aref bytes (+ i 2)) 0))))
        (write-char (char tbl (ldb (byte 6 18) triple)) out)
        (write-char (char tbl (ldb (byte 6 12) triple)) out)
        (write-char (if (< (+ i 1) n) (char tbl (ldb (byte 6 6) triple)) #\=) out)
        (write-char (if (< (+ i 2) n) (char tbl (ldb (byte 6 0) triple)) #\=) out)))
    (get-output-stream-string out)))
