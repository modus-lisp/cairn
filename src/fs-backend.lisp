;;;; fs-backend.lisp — the storage seam: where cairn's bytes actually live.
;;;;
;;;; cairn's git store (objects, refs, index, packs, config, HEAD) is a set of
;;;; named byte blobs under the git directory.  Rather than call the host
;;;; filesystem directly, the store goes through a small backend protocol —
;;;; ~9 ops on git-dir-relative string paths ("/"-separated) — so the SAME cairn
;;;; can keep a repository on the host filesystem (the default) or inside a
;;;; cabinet filesystem (which lives in a single pagetree file), which is what
;;;; lets cairn run where there is no host filesystem at all (modus).
;;;;
;;;; A backend is a vtable of closures, so cairn's core needs no dependency on
;;;; any particular backend; the cabinet backend is an opt-in add-on system
;;;; (cairn/cabinet) that supplies its own closures.

(in-package #:cairn)

(defstruct (fs-backend (:conc-name fsb-))
  read-bytes            ; (relpath)        -> (u8v | nil)      whole-file read
  read-string           ; (relpath)        -> (string | nil)  whole-file read, utf-8
  write-bytes           ; (relpath bytes)  -> t                create/replace, mkdir -p
  write-string          ; (relpath text)   -> t
  exists-p              ; (relpath)        -> boolean
  delete-file           ; (relpath)        -> t                absent is fine
  dir-exists-p          ; (reldir)         -> boolean
  list-files            ; (reldir)         -> (basename …)     files directly in reldir
  walk-files            ; (reldir)         -> (git-relpath …)  files anywhere under reldir
  ;; Optional operation-level atomicity.  A backend that can batch a whole cairn
  ;; operation (a commit, a push) into ONE store transaction supplies these; then
  ;; every write between begin and end lands together or not at all.  NIL = the
  ;; writes are independent (host/cabinet: each write is its own unit).
  begin-txn             ; ()               -> t                open a batching txn
  end-txn               ; (commit-p)        -> t                commit it, or abort
  ;; Optional streaming, for files too big to hold in memory (a fetched pack).  A
  ;; backend without them gets the whole-file fallbacks below, which are correct
  ;; but hold the file in memory.
  write-stream          ; (relpath next)   -> n-bytes          create/replace from chunks:
                        ;   NEXT returns (values u8v end) or NIL; the vector may be reused
  open-input            ; (relpath)        -> (handle | nil)   for ranged reads
  read-into             ; (handle off buf start end) -> n      0 at end of file
  input-size            ; (handle)         -> n-bytes
  close-input           ; (handle)         -> t
  rename-file           ; (from to)        -> t                replace TO
  open-output           ; (relpath)        -> handle           create/replace, pushed in pieces
  write-output          ; (handle buf start end) -> t
  close-output)         ; (handle abort)   -> t                ABORT: leave RELPATH as it was

;;; ---- the wrappers cairn's store code calls ---------------------------------

(declaim (inline repo-fsb))
(defun repo-fsb (repo) (repo-backend repo))

(defun fs-read-bytes  (repo rel)        (funcall (fsb-read-bytes  (repo-fsb repo)) rel))
(defun fs-read-string (repo rel)        (funcall (fsb-read-string (repo-fsb repo)) rel))
(defun fs-write-bytes (repo rel bytes)  (funcall (fsb-write-bytes (repo-fsb repo)) rel bytes))
(defun fs-write-string (repo rel text)  (funcall (fsb-write-string (repo-fsb repo)) rel text))
(defun fs-exists-p    (repo rel)        (funcall (fsb-exists-p    (repo-fsb repo)) rel))
(defun fs-delete-file (repo rel)        (funcall (fsb-delete-file (repo-fsb repo)) rel))
(defun fs-dir-exists-p (repo rel)       (funcall (fsb-dir-exists-p (repo-fsb repo)) rel))
(defun fs-list-files  (repo rel)        (funcall (fsb-list-files  (repo-fsb repo)) rel))
(defun fs-walk-files  (repo rel)        (funcall (fsb-walk-files  (repo-fsb repo)) rel))

;;; Streaming, with whole-file fallbacks for a backend that has no streaming of its own.

(defun fs-write-stream (repo rel next)
  "Create or replace REL with the chunks NEXT yields ((values u8v end) or NIL).  Returns
   the length.  Streams to the store when the backend can; otherwise collects the chunks."
  (let ((be (repo-fsb repo)))
    (if (fsb-write-stream be)
        (funcall (fsb-write-stream be) rel next)
        (let ((out (make-array 65536 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
          (loop (multiple-value-bind (v end) (funcall next)
                  (unless v (return))
                  (let* ((end (or end (length v))) (fp (fill-pointer out)))
                    (when (> (+ fp end) (array-dimension out 0))
                      (setf out (adjust-array out (max (+ fp end) (* 2 (array-dimension out 0))))))
                    (setf (fill-pointer out) (+ fp end))
                    (replace out v :start1 fp :end2 end))))
          (fs-write-bytes repo rel (coerce out 'u8v))
          (length out)))))

(defun fs-open-input (repo rel)
  "A handle for reading REL a range at a time (FS-READ-INTO), or NIL if it does not exist."
  (let ((be (repo-fsb repo)))
    (if (fsb-open-input be)
        (let ((h (funcall (fsb-open-input be) rel))) (and h (cons be h)))
        (let ((bytes (fs-read-bytes repo rel))) (and bytes (cons nil bytes))))))

(defun fs-read-into (h offset buf start end)
  "Read from OFFSET of the file open as H into BUF[START..END).  Returns how many octets."
  (if (car h)
      (funcall (fsb-read-into (car h)) (cdr h) offset buf start end)
      (let ((n (max 0 (min (- end start) (- (length (cdr h)) offset)))))
        (replace buf (cdr h) :start1 start :end1 (+ start n) :start2 offset)
        n)))

(defun fs-input-size (h)
  (if (car h) (funcall (fsb-input-size (car h)) (cdr h)) (length (cdr h))))

(defun fs-close-input (h)
  (when (and (car h) (fsb-close-input (car h))) (funcall (fsb-close-input (car h)) (cdr h)))
  t)

(defun fs-open-output (repo rel)
  "Begin writing REL a piece at a time (FS-WRITE-OUTPUT, FS-CLOSE-OUTPUT).  Straight to the
   store when the backend can; otherwise the pieces are collected and written at the close."
  (let ((be (repo-fsb repo)))
    (if (fsb-open-output be)
        (cons be (funcall (fsb-open-output be) rel))
        (cons nil (list repo rel (make-array 4096 :element-type '(unsigned-byte 8)
                                                  :adjustable t :fill-pointer 0))))))

(defun %collect-octets (out buf start end)
  (let ((fp (fill-pointer out)) (n (- end start)))
    (when (> (+ fp n) (array-dimension out 0))
      (setf out (adjust-array out (max (+ fp n) (* 2 (array-dimension out 0))))))
    (setf (fill-pointer out) (+ fp n))
    (replace out buf :start1 fp :start2 start :end2 end)
    out))

(defun fs-write-output (h buf start end)
  (if (car h)
      (funcall (fsb-write-output (car h)) (cdr h) buf start end)
      (setf (third (cdr h)) (%collect-octets (third (cdr h)) buf start end)))
  t)

(defun fs-close-output (h &key abort)
  (if (car h)
      (funcall (fsb-close-output (car h)) (cdr h) abort)
      (destructuring-bind (repo rel out) (cdr h)
        (unless abort (fs-write-bytes repo rel (coerce out 'u8v)))))
  t)

(defun fs-rename-file (repo from to)
  "Rename FROM to TO, replacing TO."
  (let ((be (repo-fsb repo)))
    (if (fsb-rename-file be)
        (funcall (fsb-rename-file be) from to)
        (progn (fs-write-bytes repo to (fs-read-bytes repo from))
               (fs-delete-file repo from)))
    t))

(defun loose-object-relpath (sha)
  "The git-dir-relative path of the loose object named SHA (hex)."
  (format nil "objects/~a/~a" (subseq sha 0 2) (subseq sha 2)))

(defun call-with-store-transaction (repo thunk)
  "Run THUNK with REPO's store writes batched into one atomic transaction, if the
   backend supports it (else just run THUNK — writes are independent).  On a
   non-local exit the batch is aborted, so a failed operation writes nothing."
  (let ((be (and repo (repo-backend repo))))
    (if (and be (fsb-begin-txn be))
        (progn
          (funcall (fsb-begin-txn be))
          (let ((ok nil))
            (unwind-protect (multiple-value-prog1 (funcall thunk) (setf ok t))
              (funcall (fsb-end-txn be) ok))))
        (funcall thunk))))

(defmacro with-store-transaction ((repo) &body body)
  "Batch every store write in BODY into one atomic transaction (see
   CALL-WITH-STORE-TRANSACTION).  A no-op for backends without transactions."
  `(call-with-store-transaction ,repo (lambda () ,@body)))

;;; ---- the working-tree backend ----------------------------------------------
;;;
;;; Checkout and status touch the *working tree* — real files with modes and
;;; symlinks, and the lstat metadata the index records.  That's a distinct,
;;; differently-rooted surface from the git store, so it's a second vtable held
;;; in the repository's WORKTREE slot (NIL for a bare repo).  Paths are
;;; worktree-relative strings.

(defstruct (wt-stat (:conc-name wts-))
  type (mode 0) (size 0) (mtime 0) (ctime 0) (dev 0) (ino 0) (uid 0) (gid 0))
  ;; TYPE is :file / :symlink / :dir.  DEV/INO/UID/GID are best-effort — a store
  ;; without them (cabinet) reports 0; cairn's change-detection leans on size+mtime.

(defstruct (wt-backend (:conc-name wtb-))
  read-file             ; (rel)              -> bytes           regular file content
  write-file            ; (rel bytes exec)   -> t               write regular/exec file, mkdir -p
  make-symlink          ; (rel target)       -> t               create/replace a symlink
  read-symlink          ; (rel)              -> string          a symlink's target
  delete-file           ; (rel)              -> t               absent is fine
  exists-p              ; (rel)              -> boolean         anything present at REL
  lstat                 ; (rel)              -> (wt-stat | nil) no symlink following
  walk                  ; (reldir)           -> (relpath …)     files under reldir, minus .git
  ;; Optional: a regular file written in pieces, so checkout can stream a big blob into
  ;; place.  A backend without them gets WRITE-FILE of the collected pieces.
  open-output           ; (rel exec)         -> handle
  write-output          ; (handle buf start end) -> t
  close-output)         ; (handle abort)     -> t

(defun repo-wtb (repo)
  (or (repo-worktree repo) (error "cairn: repository has no working tree (bare?)")))

(defun wt-read-file    (repo rel)          (funcall (wtb-read-file   (repo-wtb repo)) rel))
(defun wt-write-file   (repo rel bytes exec) (funcall (wtb-write-file (repo-wtb repo)) rel bytes exec))
(defun wt-make-symlink (repo rel target)   (funcall (wtb-make-symlink (repo-wtb repo)) rel target))
(defun wt-read-symlink (repo rel)          (funcall (wtb-read-symlink (repo-wtb repo)) rel))
(defun wt-delete-file  (repo rel)          (funcall (wtb-delete-file (repo-wtb repo)) rel))
(defun wt-exists-p     (repo rel)          (funcall (wtb-exists-p    (repo-wtb repo)) rel))
(defun wt-lstat        (repo rel)          (funcall (wtb-lstat       (repo-wtb repo)) rel))
(defun wt-walk         (repo rel)          (funcall (wtb-walk        (repo-wtb repo)) rel))

(defun wt-open-output (repo rel exec)
  "Begin writing the regular file REL (executable when EXEC) a piece at a time."
  (let ((wt (repo-wtb repo)))
    (if (wtb-open-output wt)
        (cons wt (funcall (wtb-open-output wt) rel exec))
        (cons nil (list repo rel exec (make-array 4096 :element-type '(unsigned-byte 8)
                                                       :adjustable t :fill-pointer 0))))))

(defun wt-write-output (h buf start end)
  (if (car h)
      (funcall (wtb-write-output (car h)) (cdr h) buf start end)
      (setf (fourth (cdr h)) (%collect-octets (fourth (cdr h)) buf start end)))
  t)

(defun wt-close-output (h &key abort)
  (if (car h)
      (funcall (wtb-close-output (car h)) (cdr h) abort)
      (destructuring-bind (repo rel exec out) (cdr h)
        (unless abort (wt-write-file repo rel (coerce out 'u8v) exec))))
  t)

;;; ---- the host filesystem backend (the default) -----------------------------

(defun host-walk-files (dir git-dir)
  "Git-dir-relative paths of every file anywhere under DIR (a pathname)."
  (let ((out '()))
    (labels ((walk (d)
               (dolist (p (uiop:directory-files d)) (push (enough-namestring p git-dir) out))
               (dolist (s (uiop:subdirectories d)) (walk s))))
      (when (uiop:directory-exists-p dir) (walk dir)))
    (nreverse out)))

(defun make-host-backend (git-dir)
  "A backend that reads and writes the real host filesystem under GIT-DIR (a
   directory pathname).  This is byte-for-byte the behavior cairn had before the
   seam existed."
  (flet ((full (rel) (merge-pathnames rel git-dir)))
    (make-fs-backend
     :read-bytes   (lambda (rel) (let ((p (full rel))) (when (probe-file p) (slurp-bytes p))))
     :read-string  (lambda (rel) (slurp-string (full rel)))
     :write-bytes  (lambda (rel bytes) (write-bytes (full rel) bytes) t)
     :write-string (lambda (rel text) (write-text-file (full rel) text) t)
     :exists-p     (lambda (rel) (and (probe-file (full rel)) t))
     :delete-file  (lambda (rel) (uiop:delete-file-if-exists (full rel)) t)
     :dir-exists-p (lambda (rel) (and (uiop:directory-exists-p (full rel)) t))
     :list-files   (lambda (rel)
                     (let ((d (full rel)))
                       (when (uiop:directory-exists-p d)
                         (mapcar #'file-namestring (uiop:directory-files d)))))
     :walk-files   (lambda (rel) (host-walk-files (full rel) git-dir))
     :write-stream (lambda (rel next)
                     (let ((p (full rel)) (n 0))
                       (ensure-directories-exist p)
                       (with-open-file (s p :element-type '(unsigned-byte 8) :direction :output
                                            :if-exists :supersede :if-does-not-exist :create)
                         (loop (multiple-value-bind (v end) (funcall next)
                                 (unless v (return))
                                 (let ((end (or end (length v))))
                                   (write-sequence v s :end end)
                                   (incf n end)))))
                       n))
     ;; A handle is an open stream, so a pack being read for a whole checkout costs one
     ;; open; CLOSE-INPUT closes it.
     :open-input   (lambda (rel)
                     (let ((p (full rel)))
                       (when (probe-file p) (open p :element-type '(unsigned-byte 8)))))
     :read-into    (lambda (s off buf start end)
                     (file-position s off)
                     (- (read-sequence buf s :start start :end end) start))
     :input-size   (lambda (s) (file-length s))
     :close-input  (lambda (s) (close s) t)
     :open-output  (lambda (rel)
                     (let ((p (full rel)))
                       (ensure-directories-exist p)
                       (open p :element-type '(unsigned-byte 8) :direction :output
                               :if-exists :supersede :if-does-not-exist :create)))
     :write-output (lambda (s buf start end) (write-sequence buf s :start start :end end) t)
     :close-output (lambda (s abort) (close s :abort abort) t)
     :rename-file  (lambda (from to)
                     (let ((to (full to)))
                       (when (probe-file to) (delete-file to))
                       (rename-file (full from) to)
                       t)))))
