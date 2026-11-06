;;;; pack.lisp — packfiles: the .idx index and the delta-compressed .pack store.
;;;;
;;;; After `git gc`, objects live in packs: a *.pack of zlib-compressed (and often
;;;; delta-compressed) objects, with a *.idx v2 giving each object's SHA-1 and its
;;;; byte offset in the pack.  Reading an object: binary-search the idx (via its
;;;; 256-way fanout) for the offset, parse the object header, and inflate — then,
;;;; for a delta object, recursively fetch its base and apply the delta.

(in-package #:cairn)

(defstruct pack path idx pack-data n)

;;; ---- where a pack's bytes come from ----------------------------------------
;;;
;;; PACK-DATA is a PACK SOURCE: either the whole pack as an octet vector, or a
;;; PACK-WINDOW over a file in the store, which holds only the stretch of the pack
;;; being read.  A clone streams the pack to disk and reads it back through a
;;; window, so neither indexing nor checkout needs the pack in memory -- only the
;;; object at hand.

(defstruct (pack-window (:constructor %make-pack-window))
  handle                                ; an FS-OPEN-INPUT handle
  (size 0)                              ; the file's length
  (buf (make-array 0 :element-type '(unsigned-byte 8)))
  (base 0)                              ; file offset of BUF[0]
  (len 0))                              ; valid octets in BUF

(defparameter *pack-window-size* 65536
  "The least a pack window reads from the store at once.")

(defun make-pack-window (handle)
  (%make-pack-window :handle handle :size (fs-input-size handle)))

(defun psrc-size (src)
  (if (pack-window-p src) (pack-window-size src) (length src)))

(defun psrc-ensure (src off n)
  "Make the N octets at OFF readable (fewer at the end of the pack).  Returns (values
   VECTOR INDEX): they are VECTOR[INDEX..INDEX+N).  The vector is only good until the
   next call on SRC."
  (if (not (pack-window-p src))
      (values src off)
      (let ((base (pack-window-base src)))
        (if (and (>= off base) (<= (+ off n) (+ base (pack-window-len src))))
            (values (pack-window-buf src) (- off base))
            (let* ((want (max 0 (min (max n *pack-window-size*) (- (pack-window-size src) off))))
                   (buf (pack-window-buf src)))
              (when (< (length buf) want)
                (setf buf (make-array (max want (* 2 (length buf))) :element-type '(unsigned-byte 8))
                      (pack-window-buf src) buf))
              (setf (pack-window-len src) 0 (pack-window-base src) off)
              (let ((got (fs-read-into (pack-window-handle src) off buf 0 want)))
                (unless (= got want)
                  (error "cairn: pack store returned ~d of ~d octets at ~d" got want off))
                (setf (pack-window-len src) got))
              (values buf 0))))))

(defun psrc-byte (src off)
  (multiple-value-bind (v i) (psrc-ensure src off 1) (aref v i)))

(defun psrc-subseq (src start end)
  (multiple-value-bind (v i) (psrc-ensure src start (- end start))
    (subseq v i (+ i (- end start)))))

(defun psrc-inflate (src pos size)
  "Inflate the zlib stream at POS, which expands to SIZE octets.  Returns (values CONTENT
   CONSUMED).  From a window, the read is sized from SIZE (zlib's own bound on how big a
   stream of SIZE octets can be), and doubled if the stream turns out longer."
  (if (not (pack-window-p src))
      (multiple-value-bind (content end) (cram:zlib-decompress src :start pos)
        (values content (- end pos)))
      (let* ((left (- (pack-window-size src) pos))
             (n (min left (+ size (ash size -12) (ash size -14) 4096))))
        (loop
          (multiple-value-bind (v i) (psrc-ensure src pos n)
            (handler-case
                (multiple-value-bind (content end) (cram:zlib-decompress v :start i :end (+ i n))
                  (return (values content (- end i))))
              (error (e)
                (if (< n left)
                    (setf n (min left (* 2 n)))
                    (error e)))))))))

(defun pack-object-header (src pos)
  "Parse the object header at POS.  Returns (values TYPE SIZE POS-AFTER)."
  (multiple-value-bind (v i) (psrc-ensure src pos 16)
    (let* ((b (aref v i)) (type (logand (ash b -4) 7)) (size (logand b 15)) (shift 4) (k 1))
      (loop while (logtest b #x80) do
        (setf b (aref v (+ i k))) (incf k)
        (setf size (logior size (ash (logand b #x7f) shift))) (incf shift 7))
      (values type size (+ pos k)))))

(defun pack-ofs-base (src pos)
  "Parse an ofs-delta's base distance at POS.  Returns (values DISTANCE POS-AFTER)."
  (multiple-value-bind (v i) (psrc-ensure src pos 16)
    (let* ((k 0) (b (aref v i)) (rel (logand b #x7f)))
      (loop while (logtest b #x80) do
        (incf k) (setf b (aref v (+ i k)))
        (setf rel (logior (ash (1+ rel) 7) (logand b #x7f))))
      (values rel (+ pos k 1)))))

(declaim (inline be32))
(defun be32 (bytes off)
  (logior (ash (aref bytes off) 24) (ash (aref bytes (+ off 1)) 16)
          (ash (aref bytes (+ off 2)) 8) (aref bytes (+ off 3))))

(defun be64 (bytes off)
  (logior (ash (be32 bytes off) 32) (be32 bytes (+ off 4))))

(defun open-pack-bytes (idx pack-data &optional (label "pack"))
  "Build a PACK from already-read .idx and .pack byte vectors (LABEL is only for
   diagnostics).  The store's backend does the reading, so this never touches a
   filesystem — packs are slurped whole either way."
  (unless (and (>= (length idx) 2) (= (aref idx 0) #xff) (= (aref idx 1) (char-code #\t)))
    (error "cairn: unsupported pack index (only v2)"))
  (make-pack :path label :idx idx :pack-data pack-data
             :n (be32 idx (+ 8 (* 255 4)))))                    ; fanout[255] = object count

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
  (let ((src (pack-pack-data pack)))
    (multiple-value-bind (type size pos) (pack-object-header src offset)
      (case type
        ((1 2 3 4) (values (pack-type-keyword type) (values (psrc-inflate src pos size))))
        (6                                                      ; ofs-delta
         (multiple-value-bind (rel pos) (pack-ofs-base src pos)
           (let ((delta (values (psrc-inflate src pos size))))
             (multiple-value-bind (btype bcontent) (pack-read-at pack (- offset rel) repo)
               (values btype (apply-delta bcontent delta))))))
        (7                                                      ; ref-delta
         (let ((base-sha (bytes->hex (psrc-subseq src pos (+ pos (oid-nbytes)))))
               (delta (values (psrc-inflate src (+ pos (oid-nbytes)) size))))
           (multiple-value-bind (btype bcontent) (read-object repo base-sha)
             (values btype (apply-delta bcontent delta)))))
        (t (error "cairn: unknown pack object type ~d" type))))))

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
