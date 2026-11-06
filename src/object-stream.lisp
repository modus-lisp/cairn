;;;; object-stream.lisp — reading a packed object without holding it in memory.
;;;;
;;;; PACK-READ-AT returns an object's content as one vector, which is fine for trees and
;;;; commits and for most blobs, and not for a 10 MB blob on a machine whose whole heap is
;;;; 88 MB: inflating it the old way held the compressed stream, the output, the output's
;;;; growth copies and a final copy at once.  Here an object is STREAMED -- inflated by
;;;; cram:zlib-inflate-stream, through a window on the pack, into a SINK (buf start end) --
;;;; to wherever it is going: a SHA-1 (indexing), a working-tree file (checkout), or, when a
;;;; delta needs it as a base, into a BASE.
;;;;
;;;; A delta copies ranges out of its base at any offset, so a base has to be readable at
;;;; random.  One no bigger than *STREAM-IN-MEMORY-LIMIT* is an octet vector; a bigger one is
;;;; a SCRATCH file in the store, one per delta depth (a chain needs its whole ancestry
;;;; readable at once, a sibling reuses its level's file), read back with ranged reads.  So
;;;; the memory an object costs is a few buffers and at most that limit per level of its
;;;; delta chain, whatever its size.

(in-package #:cairn)

(defvar *stream-in-memory-limit* (* 1024 1024)
  "The largest delta base held in memory; a bigger one goes to a scratch file in the store.")

(defparameter *stream-buffer-size* 65536)

;;; ---- sinks -------------------------------------------------------------------

(defun tee-sinks (&rest sinks)
  "A sink handing each piece to every one of SINKS (NILs ignored)."
  (let ((sinks (remove nil sinks)))
    (if (= (length sinks) 1)
        (first sinks)
        (lambda (buf start end) (dolist (s sinks) (funcall s buf start end))))))

(defun make-oid-sink (type size)
  "A sink hashing an object of TYPE and SIZE octets as git does.  Returns (values SINK FINISH),
   (FINISH) giving the hex object id once all SIZE octets have gone in."
  (let ((header (string->bytes (format nil "~(~a~) ~d~c" type size (code-char 0)))))
    (ecase *oid*
      (:sha1 (let ((s (seal:sha1-init)))
               (seal:sha1-update s header)
               (values (lambda (buf start end) (seal:sha1-update s buf start end))
                       (lambda () (bytes->hex (seal:sha1-final s))))))
      (:sha256 (let ((out (make-array size :element-type '(unsigned-byte 8))) (i 0))
                 (values (lambda (buf start end) (replace out buf :start1 i :start2 start :end2 end)
                           (incf i (- end start)))
                         (lambda () (bytes->hex (seal:sha256 (concatenate 'u8v header out))))))))))

;;; ---- bases --------------------------------------------------------------------

(defstruct (scratch-base (:constructor %make-scratch-base (handle size rel)))
  handle size rel)

(defun base-size (base)
  (if (scratch-base-p base) (scratch-base-size base) (length base)))

(defun base-read (base off buf start end)
  "Copy BASE's octets from OFF into BUF[START..END)."
  (if (scratch-base-p base)
      (let ((n (fs-read-into (scratch-base-handle base) off buf start end)))
        (unless (= n (- end start))
          (error "cairn: scratch base returned ~d of ~d octets" n (- end start))))
      (replace buf base :start1 start :end1 end :start2 off)))

(defun release-base (base)
  "Done with BASE: close a scratch file's handle (the file is reused by the next base at its depth)."
  (when (scratch-base-p base) (fs-close-input (scratch-base-handle base))))

(defun scratch-relpath (depth) (format nil "objects/pack/scratch-~d.tmp" depth))

(defun make-base-collector (repo depth size)
  "Where an object of SIZE octets goes when it is wanted as a base: an octet vector, or the
   scratch file for DEPTH.  Returns (values SINK FINISH); (FINISH) returns the base."
  (if (or (<= size *stream-in-memory-limit*) (null repo))
      (let ((out (make-array size :element-type '(unsigned-byte 8))) (i 0))
        (values (lambda (buf start end)
                  (replace out buf :start1 i :start2 start :end2 end)
                  (incf i (- end start)))
                (lambda () out)))
      (let* ((rel (scratch-relpath depth))
             (h (fs-open-output repo rel)))
        (values (lambda (buf start end) (fs-write-output h buf start end))
                (lambda ()
                  (fs-close-output h)
                  (%make-scratch-base (or (fs-open-input repo rel)
                                          (error "cairn: scratch file ~a vanished" rel))
                                      size rel))))))

(defun delete-scratch-files (repo)
  (loop for d from 0 below 64
        for rel = (scratch-relpath d)
        when (fs-exists-p repo rel) do (fs-delete-file repo rel)))

;;; ---- applying a delta as it arrives ---------------------------------------------

(defun make-delta-sink (base start-output)
  "A sink taking a delta's instruction stream a piece at a time and producing the object it
   describes from BASE.  As soon as the delta's header gives the result's size,
   (START-OUTPUT SIZE) is called and returns the sink the result goes to.  Returns (values
   SINK FINISH); (FINISH) checks the delta was complete and returns the result's size."
  (let ((state :base-size) (acc 0) (shift 0)
        (result-size 0) (written 0) (out nil)
        (op 0) (args nil) (insert 0)
        (copybuf (make-array 0 :element-type '(unsigned-byte 8))))
    (labels ((emit (buf start end)
               (funcall out buf start end)
               (incf written (- end start)))
             (copy (off len)
               (when (> (+ off len) (base-size base))
                 (error "cairn: delta copies past the end of its base"))
               (when (< (length copybuf) (min len *stream-buffer-size*))
                 (setf copybuf (make-array (min len *stream-buffer-size*) :element-type '(unsigned-byte 8))))
               (loop while (plusp len)
                     do (let ((n (min len (length copybuf))))
                          (base-read base off copybuf 0 n)
                          (emit copybuf 0 n)
                          (incf off n) (decf len n))))
             (run-copy ()
               ;; ARGS holds the copy's argument octets, in the order the op's bits name them
               (let ((off 0) (len 0) (a args))
                 (loop for bit in '(#x01 #x02 #x04 #x08) for sh in '(0 8 16 24)
                       when (logtest op bit) do (setf off (logior off (ash (pop a) sh))))
                 (loop for bit in '(#x10 #x20 #x40) for sh in '(0 8 16)
                       when (logtest op bit) do (setf len (logior len (ash (pop a) sh))))
                 (when (zerop len) (setf len #x10000))
                 (copy off len))))
      (values
       (lambda (buf start end)
         (let ((i start))
           (loop while (< i end) do
             (ecase state
               ((:base-size :result-size)
                (let ((b (aref buf i)))
                  (incf i)
                  (setf acc (logior acc (ash (logand b #x7f) shift))) (incf shift 7)
                  (unless (logtest b #x80)
                    (if (eq state :base-size)
                        (progn
                          (unless (= acc (base-size base))
                            (error "cairn: delta is for a ~d-octet base, not ~d" acc (base-size base)))
                          (setf state :result-size))
                        (progn (setf result-size acc
                                     out (funcall start-output acc)
                                     state :op)))
                    (setf acc 0 shift 0))))
               (:op
                (setf op (aref buf i)) (incf i)
                (cond ((logtest op #x80)
                       (setf args nil)
                       (if (zerop (logcount (logand op #x7f)))
                           (run-copy)
                           (setf state :args)))
                      ((plusp op) (setf insert op state :insert))
                      (t (error "cairn: invalid delta opcode 0"))))
               (:args
                (push (aref buf i) args) (incf i)
                (when (= (length args) (logcount (logand op #x7f)))
                  (setf args (nreverse args))
                  (run-copy)
                  (setf state :op)))
               (:insert
                (let ((n (min insert (- end i))))
                  (emit buf i (+ i n))
                  (incf i n) (decf insert n)
                  (when (zerop insert) (setf state :op))))))))
       (lambda ()
         (unless (and (eq state :op) (= written result-size))
           (error "cairn: delta ended early (~d of ~d octets)" written result-size))
         result-size)))))

;;; ---- streaming an object out of a pack -------------------------------------------

(defun psrc-reader (src pos)
  "A READ function (buf start end) pulling the pack source SRC from POS onward."
  (lambda (buf start end)
    (let ((n (min (- end start) *stream-buffer-size* (max 0 (- (psrc-size src) pos)))))
      (when (plusp n)
        (multiple-value-bind (v i) (psrc-ensure src pos n)
          (replace buf v :start1 start :end1 (+ start n) :start2 i))
        (incf pos n))
      n)))

(defun psrc-inflate-to (src pos sink)
  "Inflate the zlib stream at POS of SRC into SINK.  Returns (values OCTETS CONSUMED)."
  (if (pack-window-p src)
      (cram:zlib-inflate-stream (psrc-reader src pos) sink)
      (cram:zlib-inflate-to src sink :start pos)))

(defun pack-stream-object (pack offset repo start-output &optional (depth 0))
  "Stream the object at OFFSET in PACK: (START-OUTPUT TYPE SIZE) is called once, with the
   object's type keyword and size, and returns the sink its content goes to.  A delta's base is
   materialised first (MATERIALISE-PACK-OBJECT at DEPTH+1).  Returns the type."
  (let ((src (pack-pack-data pack)))
    (multiple-value-bind (type size pos) (pack-object-header src offset)
      (case type
        ((1 2 3 4)
         (let ((kw (pack-type-keyword type)))
           (psrc-inflate-to src pos (funcall start-output kw size))
           kw))
        ((6 7)
         (multiple-value-bind (btype base pos)
             (if (= type 6)
                 (multiple-value-bind (rel after) (pack-ofs-base src pos)
                   (multiple-value-bind (bt b) (materialise-pack-object pack (- offset rel) repo (1+ depth))
                     (values bt b after)))
                 (let ((sha (bytes->hex (psrc-subseq src pos (+ pos (oid-nbytes))))))
                   (multiple-value-bind (bt b) (read-object repo sha)
                     (values bt b (+ pos (oid-nbytes))))))
           (unwind-protect
                (multiple-value-bind (sink finish)
                    (make-delta-sink base (lambda (rsize) (funcall start-output btype rsize)))
                  (psrc-inflate-to src pos sink)
                  (funcall finish)
                  btype)
             (release-base base))))
        (t (error "cairn: unknown pack object type ~d" type))))))

(defun materialise-pack-object (pack offset repo depth)
  "The object at OFFSET as a base a delta can read: (values TYPE BASE)."
  (let (finish)
    (let ((type (pack-stream-object pack offset repo
                                    (lambda (type size)
                                      (declare (ignore type))
                                      (multiple-value-bind (sink fin) (make-base-collector repo depth size)
                                        (setf finish fin)
                                        sink))
                                    depth)))
      (values type (funcall finish)))))

(defun repo-object-location (repo sha)
  "(values PACK OFFSET) for SHA if it is packed, else NIL."
  (dolist (pack (repo-loaded-packs repo) nil)
    (let ((off (pack-find-offset pack sha)))
      (when off (return (values pack off))))))

(defun stream-object (repo sha start-output)
  "Stream the object SHA into the sink (START-OUTPUT TYPE SIZE) returns.  Packed objects never
   pass through memory whole; a loose one is read and handed over in one piece.  Returns the
   type."
  (with-oid (repo)
    (multiple-value-bind (pack off) (repo-object-location repo sha)
      (if pack
          (pack-stream-object pack off repo start-output)
          (multiple-value-bind (type content) (read-object repo sha)
            (funcall (funcall start-output type (length content)) content 0 (length content))
            type)))))
