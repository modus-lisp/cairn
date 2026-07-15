;;;; index-pack.lisp — turn a received packfile into a readable pack + .idx.
;;;;
;;;; A fetched pack arrives with no index: just PACK, a version, an object
;;;; count, then that many zlib streams (some of them deltas against earlier
;;;; objects), and a trailing SHA-1 of the whole thing.  To store it the way
;;;; git does, we walk it once — chipz tells us where each zlib stream ends,
;;;; so we can find object boundaries — resolve every delta to get each
;;;; object's real SHA-1, and write a v2 .idx (fanout, sorted SHAs, CRCs,
;;;; offsets).  After this the ordinary pack.lisp read path takes over.

(in-package #:cairn)

(defstruct pobj offset end kind type raw base-off base-sha sha sha-bytes crc)

(defun inflate-at (data pos uncompressed-size)
  "Inflate the zlib stream in DATA starting at POS, known to expand to
   UNCOMPRESSED-SIZE bytes.  Returns (values CONTENT BYTES-CONSUMED) — the
   consumed count locates the next object in the pack."
  (let* ((out (make-array uncompressed-size :element-type '(unsigned-byte 8)))
         (state (chipz:make-dstate 'chipz:zlib)))
    (multiple-value-bind (consumed produced) (chipz:decompress out state data :input-start pos)
      (unless (= produced uncompressed-size)
        (error "cairn: pack object inflated to ~d, header said ~d" produced uncompressed-size))
      (values out consumed))))

(defun parse-pack-object (data pos)
  "Parse the object at POS.  Returns a POBJ with its raw payload (content for a
   base object, delta bytes for a delta) and its END offset in DATA."
  (let* ((start pos)
         (b (aref data pos)) (type (logand (ash b -4) 7)) (size (logand b 15)) (shift 4))
    (incf pos)
    (loop while (logtest b #x80) do
      (setf b (aref data pos)) (incf pos)
      (setf size (logior size (ash (logand b #x7f) shift))) (incf shift 7))
    (ecase type
      ((1 2 3 4)
       (multiple-value-bind (content consumed) (inflate-at data pos size)
         (make-pobj :offset start :end (+ pos consumed) :kind :base
                    :type type :raw content)))
      (6                                                   ; ofs-delta
       (let ((base-rel (logand (aref data pos) #x7f)))
         (loop while (logtest (aref data pos) #x80) do
           (incf pos)
           (setf base-rel (logior (ash (1+ base-rel) 7) (logand (aref data pos) #x7f))))
         (incf pos)
         (multiple-value-bind (delta consumed) (inflate-at data pos size)
           (make-pobj :offset start :end (+ pos consumed) :kind :ofs-delta
                      :base-off (- start base-rel) :raw delta))))
      (7                                                   ; ref-delta
       (let ((base-sha (bytes->hex (subseq data pos (+ pos 20)))))
         (incf pos 20)
         (multiple-value-bind (delta consumed) (inflate-at data pos size)
           (make-pobj :offset start :end (+ pos consumed) :kind :ref-delta
                      :base-sha base-sha :raw delta)))))))

(defun index-pack-objects (data &optional repo)
  "Walk packfile DATA, resolve all deltas, and return a vector of POBJ with
   :sha / :sha-bytes / :crc filled in.  Verifies the trailing pack checksum.
   REPO, if given, lets a *thin* pack (from a fetch with `have`s) resolve
   ref-deltas whose base is not in the pack but already in the object store."
  (unless (and (= (aref data 0) (char-code #\P)) (= (aref data 1) (char-code #\A))
               (= (aref data 2) (char-code #\C)) (= (aref data 3) (char-code #\K)))
    (error "cairn: not a packfile (bad magic)"))
  (let* ((n (be32 data 8))
         (objs (make-array n))
         (by-offset (make-hash-table))
         (by-sha (make-hash-table :test 'equal))
         (pos 12))
    ;; pass 1 — parse every object, record its byte extent + CRC
    (dotimes (i n)
      (let ((o (parse-pack-object data pos)))
        (setf (pobj-crc o) (crc32 data (pobj-offset o) (pobj-end o))
              (aref objs i) o
              (gethash (pobj-offset o) by-offset) o
              pos (pobj-end o))))
    ;; verify pack trailer = SHA-1 of everything before it
    (let ((trailer (subseq data pos (+ pos 20))))
      (unless (equalp trailer (sha1 (subseq data 0 pos)))
        (error "cairn: pack checksum mismatch")))
    ;; pass 2 — resolve deltas (bases always precede in a non-thin pack) and hash
    (labels ((resolve (o)
               (or (pobj-sha o)
                   (multiple-value-bind (type content)
                       (ecase (pobj-kind o)
                         (:base (values (pack-type-keyword (pobj-type o)) (pobj-raw o)))
                         (:ofs-delta
                          (let ((base (gethash (pobj-base-off o) by-offset)))
                            (resolve base)
                            (values (pobj-type base) (apply-delta (pobj-raw base) (pobj-raw o)))))
                         (:ref-delta
                          (let ((base (gethash (pobj-base-sha o) by-sha)))
                            (if base
                                (values (pobj-type base) (apply-delta (pobj-raw base) (pobj-raw o)))
                                ;; thin pack: base is already in the local store
                                (multiple-value-bind (btype bcontent)
                                    (read-object (or repo (error "cairn: thin-pack base ~a needs a repo"
                                                                 (pobj-base-sha o)))
                                                 (pobj-base-sha o))
                                  (values btype (apply-delta bcontent (pobj-raw o))))))))
                     ;; store the resolved content back so dependents can reuse it
                     (setf (pobj-type o) type
                           (pobj-raw o) content
                           (pobj-sha o) (hash-object type content)
                           (pobj-sha-bytes o) (hex->bytes (pobj-sha o))
                           (gethash (pobj-sha o) by-sha) o)
                     (pobj-sha o)))))
      (loop for o across objs do (resolve o)))
    objs))

;;; ---- v2 index writer --------------------------------------------------------

(defun build-pack-index (objs pack-checksum)
  "Build the v2 .idx byte vector for the POBJ vector OBJS.  PACK-CHECKSUM is the
   pack's trailing 20-byte SHA-1."
  (let ((sorted (sort (copy-seq objs) (lambda (a b)
                                        (< (%cmp-hex (pobj-sha a) (pobj-sha b)) 0))))
        (idx (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (large '()))
    ;; header + version
    (loop for b in '(#xff #x74 #x4f #x63) do (vector-push-extend b idx))  ; \377tOc
    (%push-be32 idx 2)
    ;; fanout: cumulative count by first SHA byte
    (let ((counts (make-array 256 :initial-element 0)))
      (loop for o across sorted do (incf (aref counts (aref (pobj-sha-bytes o) 0))))
      (let ((acc 0))
        (dotimes (i 256) (incf acc (aref counts i)) (%push-be32 idx acc))))
    ;; sorted SHAs
    (loop for o across sorted do
      (loop for b across (pobj-sha-bytes o) do (vector-push-extend b idx)))
    ;; CRC32s
    (loop for o across sorted do (%push-be32 idx (pobj-crc o)))
    ;; offsets (large ones spill to the 64-bit table)
    (loop for o across sorted do
      (let ((off (pobj-offset o)))
        (if (< off #x80000000)
            (%push-be32 idx off)
            (progn (%push-be32 idx (logior #x80000000 (length large)))
                   (push off large)))))
    (dolist (off (nreverse large)) (%push-be64 idx off))
    ;; pack checksum, then the idx's own checksum
    (loop for b across pack-checksum do (vector-push-extend b idx))
    (let ((digest (sha1 (subseq idx 0 (fill-pointer idx)))))
      (loop for b across digest do (vector-push-extend b idx)))
    (coerce idx '(simple-array (unsigned-byte 8) (*)))))

(defun %cmp-hex (a b)
  "Lexicographic compare of two equal-length hex strings -> -1/0/1."
  (cond ((string< a b) -1) ((string> a b) 1) (t 0)))

(defun index-pack (pack-bytes pack-dir &optional repo)
  "Write PACK-BYTES and its computed .idx into PACK-DIR (…/objects/pack/).
   Returns (values PACK-NAME OBJECT-COUNT).  REPO lets a thin pack resolve
   deltas against objects already in the store (see index-pack-objects)."
  (let* ((objs (index-pack-objects pack-bytes repo))
         (checksum (subseq pack-bytes (- (length pack-bytes) 20)))
         (name (format nil "pack-~a" (bytes->hex checksum)))
         (idx (build-pack-index objs checksum)))
    (ensure-directories-exist pack-dir)
    (write-bytes (merge-pathnames (format nil "~a.pack" name) pack-dir) pack-bytes)
    (write-bytes (merge-pathnames (format nil "~a.idx" name) pack-dir) idx)
    (values name (length objs))))
