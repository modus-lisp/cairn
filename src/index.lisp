;;;; index.lisp — the staging area (.git/index, the "dircache").
;;;;
;;;; The index is git's binary snapshot of what the next commit will contain:
;;;; one entry per path, each with the file's lstat metadata and the SHA of its
;;;; blob.  checkout.lisp writes one after materialising a tree; add/commit read
;;;; it, mutate it, and write it back.  Format is DIRC v2: a header, sorted
;;;; fixed-size entries (padded to 8 bytes), optional extensions we skip on
;;;; read, and a trailing SHA-1 of everything before it.

(in-package #:cairn)

(defstruct (index-entry (:conc-name ie-))
  ctime mtime dev ino mode uid gid size sha path (stage 0))

;;; git index mode = 4-bit object type + 9-bit unix perms.
(defun tree-mode->index-mode (tree-mode)
  (cond ((string= tree-mode "100644") #o100644)
        ((string= tree-mode "100755") #o100755)
        ((string= tree-mode "120000") #o120000)         ; symlink
        ((string= tree-mode "160000") #o160000)         ; gitlink (submodule)
        (t (error "cairn: unexpected tree entry mode ~a" tree-mode))))

(defun index-mode->tree-mode (mode)
  (cond ((= mode #o100644) "100644") ((= mode #o100755) "100755")
        ((= mode #o120000) "120000") ((= mode #o160000) "160000")
        (t (error "cairn: unexpected index mode ~o" mode))))

(defun read-index (repo)
  "Parse .git/index into a vector of INDEX-ENTRY (empty if the file is absent)."
  (let ((buf (fs-read-bytes repo "index")))
    (unless buf (return-from read-index (make-array 0)))
    (let* ((n (be32 buf 8))
           (entries (make-array n))
           (pos 12))
      (unless (and (= (aref buf 0) (char-code #\D)) (= (aref buf 1) (char-code #\I)))
        (error "cairn: not a git index (bad DIRC signature)"))
      (dotimes (i n)
        (let* ((start pos)
               (ctime (be32 buf pos)) (mtime (be32 buf (+ pos 8)))
               (dev (be32 buf (+ pos 16))) (ino (be32 buf (+ pos 20)))
               (mode (be32 buf (+ pos 24))) (uid (be32 buf (+ pos 28)))
               (gid (be32 buf (+ pos 32))) (size (be32 buf (+ pos 36)))
               (fpos (+ pos 40 (oid-nbytes)))          ; flags follow the 40 stat bytes + sha
               (sha (bytes->hex (subseq buf (+ pos 40) fpos)))
               (flags (logior (ash (aref buf fpos) 8) (aref buf (1+ fpos))))
               (stage (logand (ash flags -12) 3))
               (namelen (logand flags #xfff))
               (name-start (+ fpos 2))
               (name-end (if (= namelen #xfff)
                             (position 0 buf :start name-start)
                             (+ name-start namelen))))
          (setf (aref entries i)
                (make-index-entry :ctime ctime :mtime mtime :dev dev :ino ino
                                  :mode mode :uid uid :gid gid :size size :sha sha
                                  :stage stage :path (ascii (subseq buf name-start name-end))))
          ;; advance past the entry, padded so its length is a multiple of 8
          (let ((len (- name-end start)))
            (setf pos (+ start (logand (+ len 8) (lognot 7)))))))
      entries)))

(defun write-index (repo entries)
  "Write ENTRIES (a sequence of INDEX-ENTRY) as a v2 .git/index.  Entries sort by
   path then stage, so conflict stages (1/2/3) sit together under their path."
  (let ((sorted (sort (coerce entries 'list)
                      (lambda (a b) (or (string< (ie-path a) (ie-path b))
                                        (and (string= (ie-path a) (ie-path b))
                                             (< (ie-stage a) (ie-stage b)))))))
        (buf (byte-buffer)))
    (push-bytes buf (string->bytes "DIRC"))
    (%push-be32 buf 2)
    (%push-be32 buf (length sorted))
    (dolist (e sorted)
      (let* ((name (string->bytes (ie-path e)))
             (start (fill-pointer buf)))
        (%push-be32 buf (logand (ie-ctime e) #xffffffff)) (%push-be32 buf 0)
        (%push-be32 buf (logand (ie-mtime e) #xffffffff)) (%push-be32 buf 0)
        (%push-be32 buf (logand (ie-dev e) #xffffffff))
        (%push-be32 buf (logand (ie-ino e) #xffffffff))
        (%push-be32 buf (ie-mode e))
        (%push-be32 buf (ie-uid e))
        (%push-be32 buf (ie-gid e))
        (%push-be32 buf (logand (ie-size e) #xffffffff))
        (push-bytes buf (hex->bytes (ie-sha e)))
        (%push-be16 buf (logior (ash (ie-stage e) 12) (min (length name) #xfff)))
        (push-bytes buf name)
        (let* ((len (- (fill-pointer buf) start))
               (pad (- (logand (+ len 8) (lognot 7)) len)))
          (dotimes (_ pad) (vector-push-extend 0 buf)))))
    (push-bytes buf (oid-digest (subseq buf 0 (fill-pointer buf))))
    (fs-write-bytes repo "index" (coerce buf 'u8v))))

(defun stat-index-entry (repo relpath sha mode)
  "Build an INDEX-ENTRY for the worktree file at repo-relative RELPATH, whose blob
   is SHA and tree-mode is the octal string MODE.  lstat metadata comes through
   the repository's worktree backend (a store without dev/ino/uid/gid reports 0)."
  (let ((st (wt-lstat repo relpath)))
    (flet ((f (reader) (if st (funcall reader st) 0)))
      (make-index-entry :ctime (f #'wts-ctime) :mtime (f #'wts-mtime)
                        :dev (f #'wts-dev) :ino (f #'wts-ino)
                        :mode (tree-mode->index-mode mode) :uid (f #'wts-uid)
                        :gid (f #'wts-gid) :size (f #'wts-size)
                        :sha sha :path relpath))))
