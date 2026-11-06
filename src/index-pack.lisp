;;;; index-pack.lisp — turn a received packfile into a readable pack + .idx.
;;;;
;;;; A fetched pack arrives with no index: just PACK, a version, an object
;;;; count, then that many zlib streams (some of them deltas against earlier
;;;; objects), and a trailing SHA-1 of the whole thing.  To store it the way
;;;; git does, we scan it once — cram tells us where each zlib stream ends, so
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

(defvar *index-pack-retain* (* 8 1024 1024)
  "How many octets of inflated objects the scan may keep for the resolve phase.  An object
   past this budget is inflated again when its delta tree is resolved, so indexing a big pack
   costs a second inflate of part of it instead of memory proportional to the pack.")

(defun %crc32-update (c bytes start end)
  "Advance the CRC-32 register C (pre-inverted) over BYTES[START..END)."
  (loop for i from start below end do
    (setf c (logxor (ash c -8) (aref *crc32-table* (logand (logxor c (aref bytes i)) #xff)))))
  c)

(defun %psrc-crc-and-hash (src start end hash)
  "CRC-32 of the pack's octets [START, END), feeding the same octets to the SHA-1 state HASH
   (when it is not NIL) on the way.  Reads a window at a time."
  (let ((c #xffffffff) (pos start))
    (loop while (< pos end) do
      (let ((n (min (- end pos) *pack-window-size*)))
        (multiple-value-bind (v i) (psrc-ensure src pos n)
          (setf c (%crc32-update c v i (+ i n)))
          (when hash (seal:sha1-update hash v i (+ i n))))
        (incf pos n)))
    (logxor c #xffffffff)))

(defun inflate-payload (src o)
  "The object's own payload -- base content, or a delta's instruction stream -- taking it
   from the scan if the scan kept it (and then letting it go), else inflating it again."
  (or (shiftf (pobj-raw o) nil)
      (multiple-value-bind (content consumed) (psrc-inflate src (pobj-payload-off o) (pobj-size o))
        (declare (ignore consumed))
        (unless (= (length content) (pobj-size o))
          (error "cairn: pack object inflated to ~d, header said ~d" (length content) (pobj-size o)))
        content)))

(defun scan-pack-object (src pos keep)
  "Parse the object header at POS and find its END by inflating its stream.  Returns a POBJ;
   the inflated payload is kept in its RAW slot only when KEEP is true."
  (multiple-value-bind (type size p) (pack-object-header src pos)
    (let ((base-off nil) (base-sha nil) (kind :base))
      (case type
        ((1 2 3 4))
        (6 (multiple-value-bind (rel after) (pack-ofs-base src p)
             (setf kind :ofs-delta base-off (- pos rel) p after)))
        (7 (setf kind :ref-delta
                 base-sha (bytes->hex (psrc-subseq src p (+ p (oid-nbytes)))))
           (incf p (oid-nbytes)))
        (t (error "cairn: unknown pack object type ~d at ~d" type pos)))
      (multiple-value-bind (content consumed) (psrc-inflate src p size)
        (unless (= (length content) size)
          (error "cairn: pack object inflated to ~d, header said ~d" (length content) size))
        (make-pobj :offset pos :end (+ p consumed) :kind kind :type type :size size
                   :payload-off p :base-off base-off :base-sha base-sha
                   :raw (and keep content))))))

(defun index-pack-objects (data &optional repo)
  "Walk the pack DATA -- an octet vector, or a PACK-WINDOW over the pack in the store --
   resolve all deltas, and return a vector of POBJ with :sha / :sha-bytes / :crc filled in.
   Verifies the trailing pack checksum.  REPO, if given, lets a *thin* pack (from a fetch
   with `have`s) resolve ref-deltas whose base is not in the pack but already in the store.

   Memory is bounded by the deepest delta chain and *INDEX-PACK-RETAIN*, not the pack: the
   scan records each object's extent and CRC, keeping inflated content only within that
   budget, and resolution is a depth-first walk of the delta tree that holds only the
   objects on the current root-to-leaf path (git's approach)."
  (let ((magic (psrc-subseq data 0 4)))
    (unless (equalp magic #(80 65 67 75))                    ; "PACK"
      (error "cairn: not a packfile (bad magic)")))
  (let* ((n (multiple-value-bind (v i) (psrc-ensure data 8 4) (be32 v i)))
         (objs (make-array n))
         (by-offset (make-hash-table))
         (by-sha (make-hash-table :test 'equal))
         (children (make-hash-table))                       ; base offset -> child indices
         (hash (and (eq *oid* :sha1) (seal:sha1-init)))
         (kept 0)
         (pos 12))
    (when hash (multiple-value-bind (v i) (psrc-ensure data 0 12) (seal:sha1-update hash v i (+ i 12))))
    ;; pass 1 -- object extents, CRCs and the pack checksum, in one sequential pass
    (dotimes (i n)
      (let* ((keep (<= kept *index-pack-retain*))
             (o (scan-pack-object data pos keep)))
        (when (pobj-raw o) (incf kept (length (pobj-raw o))))
        (setf (pobj-crc o) (%psrc-crc-and-hash data (pobj-offset o) (pobj-end o) hash))
        (setf (aref objs i) o
              (gethash (pobj-offset o) by-offset) o
              pos (pobj-end o))
        (when (eq (pobj-kind o) :ofs-delta)
          (push i (gethash (pobj-base-off o) children)))))
    (let ((trailer (psrc-subseq data pos (+ pos (oid-nbytes)))))
      (unless (equalp trailer (if hash (seal:sha1-final hash) (oid-digest (psrc-subseq data 0 pos))))
        (error "cairn: pack checksum mismatch")))
    ;; ---- resolve deltas ------------------------------------------------------
    ;; Each base object's delta subtree is independent, so an in-memory pack fans the
    ;; subtrees across a worker pool: a worker inflates a base, hashes it, and pushes its
    ;; content down to its ofs-delta children (apply-delta) recursively.  Workers only READ
    ;; the shared scan tables and write their own objects' fields.  A pack read through a
    ;; window resolves on one thread: the window is one cursor.
    (labels ((resolve-subtree (o type content)
               (setf (pobj-type o) type
                     (pobj-sha o) (hash-object type content)
                     (pobj-sha-bytes o) (hex->bytes (pobj-sha o)))
               (dolist (ci (gethash (pobj-offset o) children))
                 (let ((c (aref objs ci)))
                   (resolve-subtree c type (apply-delta content (inflate-payload data c))))))
             (root (o)
               (resolve-subtree o (pack-type-keyword (pobj-type o)) (inflate-payload data o))))
      (let* ((roots (coerce (loop for o across objs when (eq (pobj-kind o) :base) collect o)
                            'simple-vector))
             (nroots (length roots))
             (oid *oid*)
             (nthreads (if (or (< nroots 256) (pack-window-p data)) 1
                           (max 1 (min *index-pack-threads* nroots))))
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
                 (:base (values (pobj-type o) (inflate-payload data o)))
                 (:ofs-delta (multiple-value-bind (bt bc) (content-of (gethash (pobj-base-off o) by-offset))
                               (values bt (apply-delta bc (inflate-payload data o)))))
                 (:ref-delta (multiple-value-bind (bt bc) (ref-base-content o)
                               (values bt (apply-delta bc (inflate-payload data o)))))))
             (ref-base-content (o)
               (let ((base (gethash (pobj-base-sha o) by-sha)))
                 (if base (content-of base)
                     (read-object (or repo (error "cairn: thin-pack base ~a needs a repo"
                                                  (pobj-base-sha o)))
                                  (pobj-base-sha o)))))
             (rec (o type content)
               (setf (pobj-type o) type (pobj-sha o) (hash-object type content)
                     (pobj-sha-bytes o) (hex->bytes (pobj-sha o))
                     (gethash (pobj-sha o) by-sha) o)
               (dolist (ci (gethash (pobj-offset o) children))
                 (let ((c (aref objs ci)))
                   (rec c type (apply-delta content (inflate-payload data c)))))))
      (loop for o across objs when (null (pobj-sha o)) do
        (multiple-value-bind (type content)
            (if (eq (pobj-kind o) :ref-delta)
                (multiple-value-bind (bt bc) (ref-base-content o)
                  (values bt (apply-delta bc (inflate-payload data o))))
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

(defun index-pack (repo pack-bytes)
  "Write PACK-BYTES and its computed .idx into REPO's objects/pack/ (through the
   repository's storage backend).  Returns (values PACK-NAME OBJECT-COUNT).  REPO
   also lets a thin pack resolve deltas against objects already in the store."
  (let* ((objs (index-pack-objects pack-bytes repo))
         (checksum (subseq pack-bytes (- (length pack-bytes) (oid-nbytes))))
         (name (format nil "pack-~a" (bytes->hex checksum)))
         (idx (build-pack-index objs checksum)))
    (fs-write-bytes repo (format nil "objects/pack/~a.pack" name) pack-bytes)
    (fs-write-bytes repo (format nil "objects/pack/~a.idx" name) idx)
    (values name (length objs))))

;;; ---- a pack that never has to be in memory ----------------------------------

(defparameter *incoming-pack* "objects/pack/incoming.pack"
  "Where a pack being received is written before it has a name.")

(defun index-pack-file (repo rel)
  "Index the pack already in the store at REL, read back a window at a time, then move it to
   objects/pack/pack-<checksum>.pack beside its .idx.  Returns (values PACK-NAME
   OBJECT-COUNT)."
  (let ((h (or (fs-open-input repo rel) (error "cairn: no pack at ~a" rel)))
        name count idx)
    (unwind-protect
         (let* ((src (make-pack-window h))
                (objs (index-pack-objects src repo))
                (checksum (psrc-subseq src (- (psrc-size src) (oid-nbytes)) (psrc-size src))))
           (setf name (format nil "pack-~a" (bytes->hex checksum))
                 count (length objs)
                 idx (build-pack-index objs checksum)))
      (fs-close-input h))
    ;; the pack under its name first: an .idx is what makes a pack visible, so it must not
    ;; point at a pack that is not there yet
    (fs-rename-file repo rel (format nil "objects/pack/~a.pack" name))
    (fs-write-bytes repo (format nil "objects/pack/~a.idx" name) idx)
    (values name count)))

(defun receive-pack-stream (repo next)
  "Write the pack NEXT yields (chunks as (values u8v end), then NIL) into REPO's store and
   index it from there, holding no more of it in memory than a window.  Returns (values
   PACK-NAME OBJECT-COUNT OCTETS)."
  (let ((ok nil) (octets 0))
    (unwind-protect
         (progn
           (setf octets (fs-write-stream repo *incoming-pack* next))
           (multiple-value-bind (name count) (index-pack-file repo *incoming-pack*)
             (setf ok t)
             (values name count octets)))
      (unless ok (ignore-errors (fs-delete-file repo *incoming-pack*))))))
