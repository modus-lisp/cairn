;;;; pack.lisp — packfiles: the .idx index and the delta-compressed .pack store.
;;;;
;;;; After `git gc`, objects live in packs: a *.pack of zlib-compressed (and often
;;;; delta-compressed) objects, with a *.idx v2 giving each object's SHA-1 and its
;;;; byte offset in the pack.  Reading an object: binary-search the idx (via its
;;;; 256-way fanout) for the offset, parse the object header, and inflate — then,
;;;; for a delta object, recursively fetch its base and apply the delta.

(in-package #:cairn)

(defstruct pack path idx pack-data n)

(declaim (inline be32))
(defun be32 (bytes off)
  (logior (ash (aref bytes off) 24) (ash (aref bytes (+ off 1)) 16)
          (ash (aref bytes (+ off 2)) 8) (aref bytes (+ off 3))))

(defun be64 (bytes off)
  (logior (ash (be32 bytes off) 32) (be32 bytes (+ off 4))))

(defun open-pack (idx-path)
  (let* ((idx (slurp-bytes idx-path))
         (pack-path (make-pathname :type "pack" :defaults idx-path)))
    (unless (and (= (aref idx 0) #xff) (= (aref idx 1) (char-code #\t)))
      (error "cairn: unsupported pack index (only v2)"))
    (make-pack :path pack-path :idx idx :pack-data (slurp-bytes pack-path)
               :n (be32 idx (+ 8 (* 255 4))))))                 ; fanout[255] = object count

(defun %cmp-oid (bytes off sha)
  "Compare the oid-width bytes at BYTES[OFF] against SHA.  -1 / 0 / 1."
  (dotimes (i (oid-nbytes) 0)
    (let ((a (aref bytes (+ off i))) (b (aref sha i)))
      (cond ((< a b) (return -1)) ((> a b) (return 1))))))

(defun pack-find-offset (pack sha-hex)
  "The byte offset in the .pack of SHA-HEX, or NIL if not in this pack."
  (let* ((idx (pack-idx pack))
         (sha (hex->bytes sha-hex)) (fb (aref sha 0)) (w (oid-nbytes))
         (lo (if (zerop fb) 0 (be32 idx (+ 8 (* (1- fb) 4)))))
         (hi (be32 idx (+ 8 (* fb 4))))
         (sha-table 1032))                                      ; 8 + 256*4
    (loop while (< lo hi) do
      (let* ((mid (floor (+ lo hi) 2))
             (cmp (%cmp-oid idx (+ sha-table (* mid w)) sha)))
        (cond ((zerop cmp) (return-from pack-find-offset (idx-offset pack mid)))
              ((minusp cmp) (setf lo (1+ mid)))
              (t (setf hi mid)))))
    nil))

(defun idx-offset (pack i)
  (let* ((idx (pack-idx pack)) (n (pack-n pack)) (w (oid-nbytes))
         (o4 (be32 idx (+ 1032 (* n (+ w 4)) (* i 4)))))        ; after sha(w)+crc(4) per obj
    (if (logtest o4 #x80000000)
        (be64 idx (+ 1032 (* n (+ w 8)) (* (logand o4 #x7fffffff) 8)))
        o4)))

(defun pack-type-keyword (n)
  (ecase n (1 :commit) (2 :tree) (3 :blob) (4 :tag)))

(defun pack-read-at (pack offset repo)
  "Return (values TYPE-KEYWORD CONTENT-BYTES) for the object at OFFSET in PACK,
   resolving ofs/ref deltas against their base objects."
  (let* ((data (pack-pack-data pack)) (pos offset)
         (b (aref data pos)) (type (logand (ash b -4) 7)) (size (logand b 15)) (shift 4))
    (declare (ignorable size))
    (incf pos)
    (loop while (logtest b #x80) do
      (setf b (aref data pos)) (incf pos)
      (setf size (logior size (ash (logand b #x7f) shift))) (incf shift 7))
    (case type
      ((1 2 3 4) (values (pack-type-keyword type) (zlib-decompress data pos)))
      (6                                                        ; ofs-delta
       (let ((base-rel (logand (aref data pos) #x7f)))
         (loop while (logtest (aref data pos) #x80) do
           (incf pos)
           (setf base-rel (logior (ash (1+ base-rel) 7) (logand (aref data pos) #x7f))))
         (incf pos)
         (multiple-value-bind (btype bcontent) (pack-read-at pack (- offset base-rel) repo)
           (values btype (apply-delta bcontent (zlib-decompress data pos))))))
      (7                                                        ; ref-delta
       (let ((base-sha (bytes->hex (subseq data pos (+ pos (oid-nbytes))))))
         (incf pos (oid-nbytes))
         (multiple-value-bind (btype bcontent) (read-object repo base-sha)
           (values btype (apply-delta bcontent (zlib-decompress data pos))))))
      (t (error "cairn: unknown pack object type ~d" type)))))

(defun apply-delta (base delta)
  "Apply a git delta (copy-from-base / insert-literal instructions) to BASE."
  (let ((i 0) (n (length delta)))
    (flet ((varint ()
             (let ((v 0) (shift 0) (b 0))
               (loop do (setf b (aref delta i)) (incf i)
                        (setf v (logior v (ash (logand b #x7f) shift))) (incf shift 7)
                     while (logtest b #x80))
               v)))
      (varint)                                                 ; base size (unused)
      (let ((out (make-array (varint) :element-type '(unsigned-byte 8)))
            (o 0))                                             ; write cursor into OUT
        (declare (type fixnum o))
        (loop while (< i n) do
          (let ((op (aref delta i)))
            (incf i)
            (cond
              ((logtest op #x80)                               ; copy a run from BASE
               (let ((off 0) (len 0))
                 (when (logtest op #x01) (setf off (logior off (aref delta i))) (incf i))
                 (when (logtest op #x02) (setf off (logior off (ash (aref delta i) 8))) (incf i))
                 (when (logtest op #x04) (setf off (logior off (ash (aref delta i) 16))) (incf i))
                 (when (logtest op #x08) (setf off (logior off (ash (aref delta i) 24))) (incf i))
                 (when (logtest op #x10) (setf len (logior len (aref delta i))) (incf i))
                 (when (logtest op #x20) (setf len (logior len (ash (aref delta i) 8))) (incf i))
                 (when (logtest op #x40) (setf len (logior len (ash (aref delta i) 16))) (incf i))
                 (when (zerop len) (setf len #x10000))
                 (replace out base :start1 o :start2 off :end2 (+ off len))
                 (incf o len)))
              ((plusp op)                                      ; insert a literal run
               (replace out delta :start1 o :start2 i :end2 (+ i op))
               (incf o op) (incf i op))
              (t (error "cairn: invalid delta opcode 0")))))
        out))))
