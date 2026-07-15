;;;; sha1.lisp — SHA-1 (FIPS 180-4), pure Common Lisp.
;;;;
;;;; git addresses every object by the SHA-1 of "<type> <size>\0<content>".
;;;; SHA-1 is cryptographically broken and is used here ONLY as git's content
;;;; hash (collision-resistance is not relied upon), which is why it lives in
;;;; cairn rather than in the modern-crypto library natrium.

(in-package #:cairn)

(deftype u8v () '(simple-array (unsigned-byte 8) (*)))
(deftype u32 () '(unsigned-byte 32))

(declaim (inline rol32))
(defun rol32 (x n)
  (declare (type u32 x) (type (integer 0 31) n))
  (logand #xffffffff (logior (ash x n) (ash x (- n 32)))))

(defun sha1 (msg)
  "SHA-1 of byte vector MSG → fresh 20-byte big-endian digest."
  (let* ((ml (length msg))
         (bitlen (* ml 8))
         (padlen (let ((r (mod (+ ml 9) 64))) (if (zerop r) (+ ml 9) (+ ml 9 (- 64 r)))))
         (m (make-array padlen :element-type '(unsigned-byte 8) :initial-element 0))
         (h (make-array 5 :element-type 'u32 :initial-contents
              '(#x67452301 #xefcdab89 #x98badcfe #x10325476 #xc3d2e1f0)))
         (w (make-array 80 :element-type 'u32 :initial-element 0)))
    (replace m msg)
    (setf (aref m ml) #x80)
    (dotimes (i 8)                                  ; 64-bit big-endian bit length
      (setf (aref m (- padlen 1 i)) (logand #xff (ash bitlen (* -8 i)))))
    (loop for base from 0 below padlen by 64 do
      (dotimes (i 16)
        (let ((j (+ base (* i 4))))
          (setf (aref w i) (logior (ash (aref m j) 24) (ash (aref m (+ j 1)) 16)
                                   (ash (aref m (+ j 2)) 8) (aref m (+ j 3))))))
      (loop for i from 16 below 80 do
        (setf (aref w i) (rol32 (logxor (aref w (- i 3)) (aref w (- i 8))
                                        (aref w (- i 14)) (aref w (- i 16))) 1)))
      (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3)) (e (aref h 4)))
        (dotimes (i 80)
          (multiple-value-bind (f k)
              (cond ((< i 20) (values (logior (logand b c) (logand (logxor b #xffffffff) d)) #x5a827999))
                    ((< i 40) (values (logxor b c d) #x6ed9eba1))
                    ((< i 60) (values (logior (logand b c) (logand b d) (logand c d)) #x8f1bbcdc))
                    (t (values (logxor b c d) #xca62c1d6)))
            (let ((tmp (logand #xffffffff (+ (rol32 a 5) f e k (aref w i)))))
              (setf e d  d c  c (rol32 b 30)  b a  a tmp))))
        (setf (aref h 0) (logand #xffffffff (+ (aref h 0) a))
              (aref h 1) (logand #xffffffff (+ (aref h 1) b))
              (aref h 2) (logand #xffffffff (+ (aref h 2) c))
              (aref h 3) (logand #xffffffff (+ (aref h 3) d))
              (aref h 4) (logand #xffffffff (+ (aref h 4) e)))))
    (let ((out (make-array 20 :element-type '(unsigned-byte 8))))
      (dotimes (i 5)
        (let ((v (aref h i)) (o (* i 4)))
          (setf (aref out o) (logand #xff (ash v -24))
                (aref out (+ o 1)) (logand #xff (ash v -16))
                (aref out (+ o 2)) (logand #xff (ash v -8))
                (aref out (+ o 3)) (logand #xff v))))
      out)))

(defun bytes->hex (bytes)
  (string-downcase
   (with-output-to-string (s) (loop for b across bytes do (format s "~2,'0x" b)))))

(defun hex->bytes (hex)
  (let* ((n (/ (length hex) 2))
         (out (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n out)
      (setf (aref out i) (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun sha1-hex (msg) (bytes->hex (sha1 msg)))
