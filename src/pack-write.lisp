;;;; pack-write.lisp — building a packfile to send (the other half of push).
;;;;
;;;; index-pack.lisp reads a received pack; this writes one.  Given the commits
;;;; we want to send and the ones the remote already has, walk the object graph
;;;; to the closure of what's new (commits, their trees, the blobs), delta-
;;;; compress it, and emit the PACK header, the objects (each a full object or an
;;;; ofs-delta against a nearby one), and a trailing SHA-1.  The delta encoder
;;;; (diff-delta) is the inverse of pack.lisp's apply-delta; the selection
;;;; heuristic is a windowed longest-match search — simpler than git's Rabin
;;;; fingerprints, but it lands within a few percent of git's pack size.

(in-package #:cairn)

(defun reachable-objects (repo start-commit &optional (seen (make-hash-table :test 'equal)))
  "Add to SEEN (a sha→type-keyword table) every object reachable from
   START-COMMIT — the commit, its ancestors, and all their trees and blobs.
   The commit history is walked with an explicit worklist (not recursion), so a
   deep linear history — tens of thousands of commits — cannot overflow the
   stack.  Returns SEEN."
  (labels ((visit-tree (sha)                             ; tree depth is bounded
             (unless (gethash sha seen)
               (setf (gethash sha seen) :tree)
               (dolist (e (parse-tree (object-data repo sha)))
                 (cond ((string= (tree-entry-mode e) "40000") (visit-tree (tree-entry-sha e)))
                       ((string= (tree-entry-mode e) "160000"))   ; submodule commit: not ours
                       (t (setf (gethash (tree-entry-sha e) seen) :blob)))))))
    (let ((queue (list start-commit)))
      (loop while queue do
        (let ((sha (pop queue)))
          (when (and sha (not (gethash sha seen)))
            (setf (gethash sha seen) :commit)
            (let ((c (parse-commit (object-data repo sha))))
              (visit-tree (commit-tree c))
              (dolist (p (commit-parents c)) (push p queue))))))))
  seen)

(defun objects-to-send (repo new-commit have-commits)
  "SHAs reachable from NEW-COMMIT but not from any of HAVE-COMMITS (a list of
   commit SHAs the remote already has; NILs ignored)."
  (let ((have (make-hash-table :test 'equal)))
    (dolist (h have-commits) (when h (reachable-objects repo h have)))
    (let ((want (reachable-objects repo new-commit))
          (out '()))
      (maphash (lambda (sha type) (declare (ignore type))
                 (unless (gethash sha have) (push sha out)))
               want)
      out)))

;;; ---- delta encoding ---------------------------------------------------------
;;;
;;; A delta rewrites TARGET as instructions against BASE: copy a run of bytes
;;; from the base, or insert literal bytes.  It's the inverse of apply-delta
;;; (pack.lisp).  We index every 16-byte block of the base by a hash, then walk
;;; the target greedily taking the longest base match at each point — a simple
;;; but effective encoder (git's is a refined Rabin-fingerprint version).

(defconstant +delta-block+ 16)

(defun write-delta-varint (out n)
  "The little-endian 7-bits-per-byte varint git uses for delta header sizes."
  (loop (let ((b (logand n #x7f)))
          (setf n (ash n -7))
          (cond ((plusp n) (vector-push-extend (logior b #x80) out))
                (t (vector-push-extend b out) (return))))))

(defun block-hash (v pos)
  (let ((h 0))
    (declare (type (unsigned-byte 32) h))
    (dotimes (k +delta-block+ h)
      (setf h (logand #xffffffff (+ (* h 31) (aref v (+ pos k))))))))

(defun %match-len (base bpos target tpos)
  (let ((n 0) (blen (length base)) (tlen (length target)))
    (loop while (and (< n #xffffff) (< (+ bpos n) blen) (< (+ tpos n) tlen)
                     (= (aref base (+ bpos n)) (aref target (+ tpos n))))
          do (incf n))
    n))

(defun emit-copy (out off len)
  "Emit copy-from-base ops for LEN bytes at base offset OFF (split at 0xffffff)."
  (loop while (plusp len) do
    (let ((chunk (min len #xffffff)) (op #x80) (payload (byte-buffer)))
      (when (plusp (logand off #xff))       (setf op (logior op #x01)) (vector-push-extend (logand off #xff) payload))
      (when (plusp (logand off #xff00))     (setf op (logior op #x02)) (vector-push-extend (logand (ash off -8) #xff) payload))
      (when (plusp (logand off #xff0000))   (setf op (logior op #x04)) (vector-push-extend (logand (ash off -16) #xff) payload))
      (when (plusp (logand off #xff000000)) (setf op (logior op #x08)) (vector-push-extend (logand (ash off -24) #xff) payload))
      (when (plusp (logand chunk #xff))     (setf op (logior op #x10)) (vector-push-extend (logand chunk #xff) payload))
      (when (plusp (logand chunk #xff00))   (setf op (logior op #x20)) (vector-push-extend (logand (ash chunk -8) #xff) payload))
      (when (plusp (logand chunk #xff0000)) (setf op (logior op #x40)) (vector-push-extend (logand (ash chunk -16) #xff) payload))
      (vector-push-extend op out) (push-bytes out payload)
      (decf len chunk) (incf off chunk))))

(defun diff-delta (base target)
  "A git-format delta that turns BASE into TARGET (apply-delta inverts it)."
  (let ((out (byte-buffer)) (blen (length base)) (tlen (length target)) (index (make-hash-table)))
    (write-delta-varint out blen)
    (write-delta-varint out tlen)
    (when (>= blen +delta-block+)                          ; index base blocks (cap chain length)
      (loop for p from 0 to (- blen +delta-block+) do
        (let ((h (block-hash base p)))
          (when (< (length (gethash h index)) 8) (push p (gethash h index))))))
    (let ((tp 0) (lit-start 0))
      (flet ((flush (end)
               (loop for s from lit-start below end by 127 do
                 (let ((n (min 127 (- end s))))
                   (vector-push-extend n out)
                   (loop for k from s below (+ s n) do (vector-push-extend (aref target k) out))))))
        (loop while (< tp tlen) do
          (let ((best-off -1) (best-len 0))
            (when (<= (+ tp +delta-block+) tlen)
              (dolist (cand (gethash (block-hash target tp) index))
                (let ((len (%match-len base cand target tp)))
                  (when (> len best-len) (setf best-len len best-off cand)))))
            (if (>= best-len +delta-block+)
                (progn (flush tp) (emit-copy out best-off best-len)
                       (incf tp best-len) (setf lit-start tp))
                (incf tp))))
        (flush tlen)))
    (coerce out 'u8v)))

;;; ---- packfile writing (with delta compression) ------------------------------

(defun write-obj-header (buf tnum size)
  "The variable-length type + uncompressed-size header of a pack object."
  (let ((byte (logior (ash tnum 4) (logand size #xf))))
    (setf size (ash size -4))
    (loop while (plusp size) do
      (vector-push-extend (logior byte #x80) buf) (setf byte (logand size #x7f) size (ash size -7)))
    (vector-push-extend byte buf)))

(defun emit-ofs (buf value)
  "Encode an ofs-delta backward distance (this-offset − base-offset > 0)."
  (let ((bytes (list (logand value #x7f))) (o (ash value -7)))
    (loop while (plusp o) do
      (decf o) (push (logior #x80 (logand o #x7f)) bytes) (setf o (ash o -7)))
    (dolist (b bytes) (vector-push-extend b buf))))

(defstruct (pw (:conc-name pw-)) sha tnum content (depth 0) base delta offset)

(defun deltify-objects (objs window max-depth)
  "Reorder OBJS (a vector of PW) for delta locality and, for each, try to encode
   it as a delta against a nearby earlier object of the same type — keeping the
   smallest delta that beats the raw object and respects MAX-DEPTH."
  (let ((objs (sort objs (lambda (a b)
                           (if (/= (pw-tnum a) (pw-tnum b)) (< (pw-tnum a) (pw-tnum b))
                               (> (length (pw-content a)) (length (pw-content b))))))))
    (loop for i from 0 below (length objs)
          for obj = (aref objs i) do
      (let ((best nil) (best-base nil))
        (loop for j from (max 0 (- i window)) below i
              for base = (aref objs j) do
          (when (and (= (pw-tnum base) (pw-tnum obj)) (< (pw-depth base) max-depth))
            (let ((d (diff-delta (pw-content base) (pw-content obj))))
              (when (and (< (length d) (length (pw-content obj)))
                         (or (null best) (< (length d) (length best))))
                (setf best d best-base j)))))
        (when best
          (setf (pw-delta obj) best (pw-base obj) best-base
                (pw-depth obj) (1+ (pw-depth (aref objs best-base)))))))
    objs))

(defun write-packfile (repo shas &key (deltify t) (window 10) (max-depth 50))
  "Serialise the objects named by SHAS into a self-contained packfile.  With
   DELTIFY (the default) objects are delta-compressed (ofs-delta) against nearby
   same-type objects, as git does; otherwise every object is stored in full."
  (let ((objs (map 'vector (lambda (sha)
                             (multiple-value-bind (type content) (read-object repo sha)
                               (make-pw :sha sha :content content
                                        :tnum (ecase type (:commit 1) (:tree 2) (:blob 3) (:tag 4)))))
                   shas))
        (buf (byte-buffer)))
    (when deltify (setf objs (deltify-objects objs window max-depth)))
    (push-bytes buf (string->bytes "PACK"))
    (%push-be32 buf 2)
    (%push-be32 buf (length objs))
    (loop for obj across objs do
      (setf (pw-offset obj) (fill-pointer buf))
      (cond ((pw-delta obj)
             (let ((base (aref objs (pw-base obj))))
               (write-obj-header buf 6 (length (pw-delta obj)))            ; OBJ_OFS_DELTA
               (emit-ofs buf (- (pw-offset obj) (pw-offset base)))
               (push-bytes buf (zlib-compress (pw-delta obj)))))
            (t (write-obj-header buf (pw-tnum obj) (length (pw-content obj)))
               (push-bytes buf (zlib-compress (pw-content obj))))))
    (push-bytes buf (oid-digest (subseq buf 0 (fill-pointer buf))))
    (coerce buf 'u8v)))
