;;;; pack-write.lisp — building a packfile to send (the other half of push).
;;;;
;;;; index-pack.lisp reads a received pack; this writes one.  Given the commits
;;;; we want to send and the ones the remote already has, walk the object graph
;;;; to the closure of what's new (commits, their trees, the blobs), and emit a
;;;; packfile: the PACK header, each object as a type/size header + its
;;;; zlib-compressed content (no deltas — a "thick" pack the server will index),
;;;; and a trailing SHA-1.  Correct, if larger than git's delta-compressed pack.

(in-package #:cairn)

(defun reachable-objects (repo start-commit &optional (seen (make-hash-table :test 'equal)))
  "Add to SEEN (a sha→type-keyword table) every object reachable from
   START-COMMIT — the commit, its ancestors, and all their trees and blobs.
   Returns SEEN."
  (labels ((visit-tree (sha)
             (unless (gethash sha seen)
               (setf (gethash sha seen) :tree)
               (dolist (e (parse-tree (object-data repo sha)))
                 (cond ((string= (tree-entry-mode e) "40000") (visit-tree (tree-entry-sha e)))
                       ((string= (tree-entry-mode e) "160000"))   ; submodule commit: not ours
                       (t (setf (gethash (tree-entry-sha e) seen) :blob))))))
           (visit-commit (sha)
             (when (and sha (not (gethash sha seen)))
               (setf (gethash sha seen) :commit)
               (let ((c (parse-commit (object-data repo sha))))
                 (visit-tree (commit-tree c))
                 (dolist (p (commit-parents c)) (visit-commit p))))))
    (visit-commit start-commit))
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

(defun write-pack-object (buf type content)
  "Append one object to packfile buffer BUF: variable-length type/size header,
   then the zlib-compressed CONTENT."
  (let* ((tnum (ecase type (:commit 1) (:tree 2) (:blob 3) (:tag 4)))
         (size (length content))
         (byte (logior (ash tnum 4) (logand size #xf))))
    (setf size (ash size -4))
    (loop while (plusp size) do
      (vector-push-extend (logior byte #x80) buf)      ; continuation bit
      (setf byte (logand size #x7f) size (ash size -7)))
    (vector-push-extend byte buf)                      ; final byte
    (push-bytes buf (zlib-compress content))))

(defun write-packfile (repo shas)
  "Serialise the objects named by SHAS into a packfile byte vector."
  (let ((buf (byte-buffer)))
    (push-bytes buf (string->bytes "PACK"))
    (%push-be32 buf 2)
    (%push-be32 buf (length shas))
    (dolist (sha shas)
      (multiple-value-bind (type content) (read-object repo sha)
        (write-pack-object buf type content)))
    (push-bytes buf (sha1 (subseq buf 0 (fill-pointer buf))))
    (coerce buf 'u8v)))
