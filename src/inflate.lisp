;;;; inflate.lisp — DEFLATE (RFC 1951) decompression + zlib (RFC 1950) wrapper.
;;;;
;;;; From scratch, no chipz/zlib.  git stores every loose object and every pack
;;;; stream as zlib-wrapped DEFLATE, so this is the codec cairn reads through.
;;;; Structure follows Mark Adler's puff.c: an LSB-first bit reader, canonical
;;;; Huffman decode, and LZ77 back-references copied out of the growing output.

(in-package #:cairn)

;;; ---- bit reader (bits are packed LSB-first within each byte) ----------------
(defstruct (bitr (:constructor make-bitr (data)))
  (data nil :type u8v) (pos 0 :type fixnum) (acc 0 :type integer) (cnt 0 :type fixnum))

(defun getbits (br n)
  "Read N bits, least-significant first."
  (loop while (< (bitr-cnt br) n) do
    (setf (bitr-acc br) (logior (bitr-acc br) (ash (aref (bitr-data br) (bitr-pos br)) (bitr-cnt br))))
    (incf (bitr-pos br)) (incf (bitr-cnt br) 8))
  (prog1 (logand (bitr-acc br) (1- (ash 1 n)))
    (setf (bitr-acc br) (ash (bitr-acc br) (- n)))
    (decf (bitr-cnt br) n)))

(defun align-byte (br)
  "Advance to the next byte boundary, returning buffered whole bytes to the input."
  (decf (bitr-pos br) (floor (bitr-cnt br) 8))
  (setf (bitr-acc br) 0 (bitr-cnt br) 0))

;;; ---- canonical Huffman ------------------------------------------------------
(defstruct huff counts symbols)

(defun build-huffman (lengths count)
  "Build a decode table from an array of code LENGTHS (COUNT symbols)."
  (let ((counts (make-array 16 :initial-element 0))
        (offs (make-array 16 :initial-element 0))
        (symbols (make-array count :initial-element 0)))
    (dotimes (s count) (incf (aref counts (aref lengths s))))
    (loop for len from 2 to 15 do
      (setf (aref offs len) (+ (aref offs (1- len)) (aref counts (1- len)))))
    (dotimes (s count)
      (when (plusp (aref lengths s))
        (setf (aref symbols (aref offs (aref lengths s))) s)
        (incf (aref offs (aref lengths s)))))
    (make-huff :counts counts :symbols symbols)))

(defun decode-symbol (br huff)
  (let ((code 0) (first 0) (index 0)
        (counts (huff-counts huff)) (symbols (huff-symbols huff)))
    (loop for len from 1 to 15 do
      (setf code (logior code (getbits br 1)))
      (let ((count (aref counts len)))
        (when (< (- code first) count)
          (return-from decode-symbol (aref symbols (+ index (- code first)))))
        (incf index count) (incf first count)
        (setf first (ash first 1) code (ash code 1))))
    (error "cairn: invalid Huffman code")))

;;; ---- length / distance tables (RFC 1951 §3.2.5) -----------------------------
(defparameter *lbase*
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter *lext*
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter *dbase*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
    1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter *dext*
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
(defparameter *clcidx* #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(defparameter *fixed-lit*
  (build-huffman (let ((l (make-array 288)))
                   (loop for i from 0 to 143 do (setf (aref l i) 8))
                   (loop for i from 144 to 255 do (setf (aref l i) 9))
                   (loop for i from 256 to 279 do (setf (aref l i) 7))
                   (loop for i from 280 to 287 do (setf (aref l i) 8))
                   l)
                 288))
(defparameter *fixed-dist* (build-huffman (make-array 30 :initial-element 5) 30))

;;; ---- block decoders ---------------------------------------------------------
(defun inflate-stored (br out)
  (align-byte br)
  (let* ((d (bitr-data br)) (p (bitr-pos br))
         (len (logior (aref d p) (ash (aref d (+ p 1)) 8))))
    (setf (bitr-pos br) (+ p 4))                    ; LEN(2) + NLEN(2)
    (dotimes (i len) (vector-push-extend (aref d (+ (bitr-pos br) i)) out))
    (incf (bitr-pos br) len)))

(defun inflate-huffman-block (br out lit dist)
  (loop for sym = (decode-symbol br lit) do
    (cond ((< sym 256) (vector-push-extend sym out))
          ((= sym 256) (return))
          (t (let* ((li (- sym 257))
                    (len (+ (aref *lbase* li) (getbits br (aref *lext* li))))
                    (ds (decode-symbol br dist))
                    (back (+ (aref *dbase* ds) (getbits br (aref *dext* ds))))
                    (start (- (fill-pointer out) back)))
               (dotimes (i len) (vector-push-extend (aref out (+ start i)) out)))))))

(defun read-dynamic-tables (br)
  (let* ((hlit (+ 257 (getbits br 5)))
         (hdist (+ 1 (getbits br 5)))
         (hclen (+ 4 (getbits br 4)))
         (cl (make-array 19 :initial-element 0)))
    (dotimes (i hclen) (setf (aref cl (aref *clcidx* i)) (getbits br 3)))
    (let ((clh (build-huffman cl 19))
          (lengths (make-array (+ hlit hdist) :initial-element 0))
          (i 0))
      (loop while (< i (+ hlit hdist)) do
        (let ((sym (decode-symbol br clh)))
          (cond ((< sym 16) (setf (aref lengths i) sym) (incf i))
                ((= sym 16) (let ((rep (+ 3 (getbits br 2))) (prev (aref lengths (1- i))))
                              (dotimes (j rep) (setf (aref lengths i) prev) (incf i))))
                ((= sym 17) (let ((rep (+ 3 (getbits br 3))))
                              (dotimes (j rep) (setf (aref lengths i) 0) (incf i))))
                (t (let ((rep (+ 11 (getbits br 7))))
                     (dotimes (j rep) (setf (aref lengths i) 0) (incf i)))))))
      (values (build-huffman (subseq lengths 0 hlit) hlit)
              (build-huffman (subseq lengths hlit) hdist)))))

;;; ---- entry points -----------------------------------------------------------
(defun inflate (data &optional (start 0))
  "Decompress raw DEFLATE from DATA starting at byte START.  Returns (values
   OUTPUT-BYTES END-POSITION)."
  (let ((br (make-bitr data))
        (out (make-array 8192 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (setf (bitr-pos br) start)
    (loop
      (let ((bfinal (getbits br 1)) (btype (getbits br 2)))
        (ecase btype
          (0 (inflate-stored br out))
          (1 (inflate-huffman-block br out *fixed-lit* *fixed-dist*))
          (2 (multiple-value-bind (lit dist) (read-dynamic-tables br)
               (inflate-huffman-block br out lit dist))))
        (when (= bfinal 1) (return))))
    (values (coerce out '(simple-array (unsigned-byte 8) (*))) (bitr-pos br))))

(defun zlib-decompress (data &optional (start 0))
  "Decompress a zlib stream (RFC 1950): 2-byte header, DEFLATE, adler32 trailer."
  (inflate data (+ start 2)))

;;; deflate / zlib-compress land in a later phase (writing objects).
(defun deflate (bytes) (declare (ignore bytes)) (error "cairn: deflate not yet implemented"))
(defun zlib-compress (bytes) (declare (ignore bytes)) (error "cairn: zlib-compress not yet implemented"))
