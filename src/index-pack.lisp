;;;; index-pack.lisp — turn a received packfile into a readable pack + .idx.
;;;;
;;;; A fetched pack arrives with no index: just PACK, a version, an object
;;;; count, then that many zlib streams (some of them deltas against earlier
;;;; objects), and a trailing SHA-1 of the whole thing.  To store it the way
;;;; git does, we scan it once — chipz tells us where each zlib stream ends, so
;;;; we can find object boundaries — recording each object's extent and CRC but
;;;; keeping *no* content; then we resolve deltas with a depth-first walk of the
;;;; delta tree, holding only the objects on the current root-to-leaf path.  So
;;;; memory is bounded by the deepest delta chain, not the pack: a big repo no
;;;; longer needs its whole expanded history resident at once.  Finally we write
;;;; a v2 .idx (fanout, sorted SHAs, CRCs, offsets) and the pack.lisp read path
;;;; takes over.

(in-package #:cairn)

(defvar *index-pack-threads* 8
  "Worker threads for parallel delta resolution in index-pack.  A fixed default,
   not a probe of the machine's core count — auto-detection means shelling out to
   `nproc` or an OS-specific syscall, and we would rather not tie the resolver to
   any particular kernel.  This only affects speed, never the result; raise it on
   a big host for more throughput.  Small packs resolve single-threaded anyway.")

(defstruct pobj offset end kind type size payload-off base-off base-sha sha sha-bytes crc raw)

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

(defun inflate-payload (data o)
  "The object's own payload — base content, or a delta's instruction stream."
  (values (inflate-at data (pobj-payload-off o) (pobj-size o))))

(defun scan-pack-object (data pos)
  "Parse the object header at POS and locate its END (by inflating to find the
   zlib stream boundary, discarding the output — so the scan holds only one
   object's worth of memory).  Returns a POBJ with structure but no content."
  (let* ((start pos)
         (b (aref data pos)) (type (logand (ash b -4) 7)) (size (logand b 15)) (shift 4))
    (incf pos)
    (loop while (logtest b #x80) do
      (setf b (aref data pos)) (incf pos)
      (setf size (logior size (ash (logand b #x7f) shift))) (incf shift 7))
    (flet ((pobj (&rest args)                              ; inflate the payload once, keep it
             (multiple-value-bind (content consumed) (inflate-at data pos size)
               (apply #'make-pobj :offset start :end (+ pos consumed) :size size
                      :payload-off pos :raw content args))))
      (ecase type
        ((1 2 3 4) (pobj :kind :base :type type))
        (6                                                 ; ofs-delta
         (let ((base-rel (logand (aref data pos) #x7f)))
           (loop while (logtest (aref data pos) #x80) do
             (incf pos)
             (setf base-rel (logior (ash (1+ base-rel) 7) (logand (aref data pos) #x7f))))
           (incf pos)
           (pobj :kind :ofs-delta :base-off (- start base-rel))))
        (7                                                 ; ref-delta
         (let ((base-sha (bytes->hex (subseq data pos (+ pos (oid-nbytes))))))
           (incf pos (oid-nbytes))
           (pobj :kind :ref-delta :base-sha base-sha)))))))

(defun index-pack-objects (data &optional repo)
  "Walk packfile DATA, resolve all deltas, and return a vector of POBJ with
   :sha / :sha-bytes / :crc filled in.  Verifies the trailing pack checksum.
   REPO, if given, lets a *thin* pack (from a fetch with `have`s) resolve
   ref-deltas whose base is not in the pack but already in the object store.

   Memory is bounded by the deepest delta chain, not the pack size: a scan
   records object boundaries + CRCs without keeping any content, and resolution
   is a depth-first walk of the delta tree that holds only the objects on the
   current root-to-leaf path (git's approach).  So a large repo no longer needs
   the whole expanded history resident at once."
  (unless (and (= (aref data 0) (char-code #\P)) (= (aref data 1) (char-code #\A))
               (= (aref data 2) (char-code #\C)) (= (aref data 3) (char-code #\K)))
    (error "cairn: not a packfile (bad magic)"))
  (let* ((n (be32 data 8))
         (objs (make-array n))
         (by-offset (make-hash-table))
         (by-sha (make-hash-table :test 'equal))
         (children (make-hash-table))                       ; base offset -> child indices
         (pos 12))
    ;; pass 1 — scan boundaries + CRC, retaining no content
    (dotimes (i n)                                            ; CRC deferred to the parallel phase
      (let ((o (scan-pack-object data pos)))
        (setf (aref objs i) o
              (gethash (pobj-offset o) by-offset) o
              pos (pobj-end o))
        (when (eq (pobj-kind o) :ofs-delta)
          (push i (gethash (pobj-base-off o) children)))))
    (let ((trailer (subseq data pos (+ pos (oid-nbytes)))))
      (unless (equalp trailer (oid-digest (subseq data 0 pos)))
        (error "cairn: pack checksum mismatch")))
    ;; ---- resolve deltas ------------------------------------------------------
    ;; Each base object's delta subtree is independent, so we fan the subtrees
    ;; across a worker pool: a worker inflates a base, hashes it, and pushes its
    ;; content down to its ofs-delta children (apply-delta) recursively.  During
    ;; this phase workers only READ the shared scan tables and write their own
    ;; objects' fields — no shared writes, no locks — so it scales across cores.
    (labels ((resolve-subtree (o type content)
               (setf (pobj-type o) type
                     (pobj-crc o) (crc32 data (pobj-offset o) (pobj-end o))
                     (pobj-sha o) (hash-object type content)
                     (pobj-sha-bytes o) (hex->bytes (pobj-sha o)))
               (dolist (ci (gethash (pobj-offset o) children))
                 (let ((c (aref objs ci)))
                   (resolve-subtree c type (apply-delta content (pobj-raw c))))))
             (root (o)
               (resolve-subtree o (pack-type-keyword (pobj-type o)) (pobj-raw o))))
      (let* ((roots (coerce (loop for o across objs when (eq (pobj-kind o) :base) collect o)
                            'simple-vector))
             (nroots (length roots))
             (oid *oid*)
             (nthreads (if (< nroots 256) 1 (max 1 (min *index-pack-threads* nroots))))
             (next 0) (lock (sb-thread:make-mutex)) (err nil))
        (if (= nthreads 1)
            (loop for o across roots do (root o))
            (flet ((worker ()
                     (let ((*oid* oid))
                       (handler-case
                           (loop for start = (sb-thread:with-mutex (lock)
                                               (when (< next nroots) (prog1 next (incf next 64))))
                                 while start do
                                   (loop for i from start below (min nroots (+ start 64))
                                         do (root (aref roots i))))
                         (error (e) (sb-thread:with-mutex (lock) (unless err (setf err e))))))))
              (mapc #'sb-thread:join-thread
                    (loop repeat nthreads collect (sb-thread:make-thread #'worker)))
              (when err (error err))))))
    ;; the rare ref-deltas (thin-pack fetches / non-ofs packs) resolve against the
    ;; now-known SHAs, sequentially
    (loop for o across objs when (pobj-sha o) do (setf (gethash (pobj-sha o) by-sha) o))
    (labels ((content-of (o)
               (ecase (pobj-kind o)
                 (:base (values (pobj-type o) (pobj-raw o)))
                 (:ofs-delta (multiple-value-bind (bt bc) (content-of (gethash (pobj-base-off o) by-offset))
                               (values bt (apply-delta bc (pobj-raw o)))))
                 (:ref-delta (multiple-value-bind (bt bc) (ref-base-content o)
                               (values bt (apply-delta bc (pobj-raw o)))))))
             (ref-base-content (o)
               (let ((base (gethash (pobj-base-sha o) by-sha)))
                 (if base (content-of base)
                     (read-object (or repo (error "cairn: thin-pack base ~a needs a repo"
                                                  (pobj-base-sha o)))
                                  (pobj-base-sha o)))))
             (rec (o type content)
               (setf (pobj-type o) type (pobj-sha o) (hash-object type content)
                     (pobj-crc o) (crc32 data (pobj-offset o) (pobj-end o))
                     (pobj-sha-bytes o) (hex->bytes (pobj-sha o))
                     (gethash (pobj-sha o) by-sha) o)
               (dolist (ci (gethash (pobj-offset o) children))
                 (let ((c (aref objs ci)))
                   (rec c type (apply-delta content (pobj-raw c)))))))
      (loop for o across objs when (null (pobj-sha o)) do
        (multiple-value-bind (type content)
            (if (eq (pobj-kind o) :ref-delta)
                (multiple-value-bind (bt bc) (ref-base-content o)
                  (values bt (apply-delta bc (pobj-raw o))))
                (content-of o))
          (rec o type content))))
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
    (let ((digest (oid-digest (subseq idx 0 (fill-pointer idx)))))
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
         (checksum (subseq pack-bytes (- (length pack-bytes) (oid-nbytes))))
         (name (format nil "pack-~a" (bytes->hex checksum)))
         (idx (build-pack-index objs checksum)))
    (ensure-directories-exist pack-dir)
    (write-bytes (merge-pathnames (format nil "~a.pack" name) pack-dir) pack-bytes)
    (write-bytes (merge-pathnames (format nil "~a.idx" name) pack-dir) idx)
    (values name (length objs))))
