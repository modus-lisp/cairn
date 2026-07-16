;;;; backend-pagetree.lisp — a cairn git store directly on pagetree KV.
;;;;
;;;; The cabinet backend keeps the git store in a *filesystem* that itself lives
;;;; in a pagetree file.  But git's object/ref store is already a key-value store
;;;; (sha -> bytes, refname -> sha), so there's no need for the filesystem layer
;;;; in between: this backend maps each git-store path straight to a pagetree key
;;;; (key = the path bytes, value = the file bytes).  Simpler and faster — no
;;;; inodes, no path resolution, no block splitting — and it unlocks the real
;;;; prize: operation-level atomicity.  A whole commit or push runs inside ONE
;;;; pagetree write transaction (begin-txn/end-txn), so its objects and its ref
;;;; move land together or not at all — something the on-disk git layout, and the
;;;; per-write cabinet backend, cannot give you.
;;;;
;;;; Bare (store only): pair with a cabinet/host worktree if you need one.

(in-package #:cairn)

(defstruct (ptbe (:conc-name ptbe-)) store (txn nil) (prefix ""))

(defun %ptbe-key (be rel)
  (string->bytes (concatenate 'string (ptbe-prefix be) rel)))

(defun %bytes-prefix-p (k pb)
  (and (>= (length k) (length pb))
       (loop for i below (length pb) always (= (aref k i) (aref pb i)))))

(defmacro %in-read ((txn be) &body body)
  "Bind TXN to BE's active batch txn if one is open, else a fresh read txn."
  (let ((b (gensym)))
    `(let ((,b ,be))
       (if (ptbe-txn ,b) (let ((,txn (ptbe-txn ,b))) ,@body)
           (pagetree:with-read-txn (,txn (ptbe-store ,b)) ,@body)))))

(defmacro %in-write ((txn be) &body body)
  "Bind TXN to BE's active batch txn if one is open, else a fresh write txn."
  (let ((b (gensym)))
    `(let ((,b ,be))
       (if (ptbe-txn ,b) (let ((,txn (ptbe-txn ,b))) ,@body)
           (pagetree:with-write-txn (,txn (ptbe-store ,b)) ,@body)))))

(defun %ptbe-scan (be reldir fn)
  "Call FN with each git-relative path whose key falls under RELDIR (a prefix)."
  (let* ((full (concatenate 'string (ptbe-prefix be) reldir))
         (pb (string->bytes full))
         (plen (length (ptbe-prefix be))))
    (%in-read (txn be)
      (let ((cur (pagetree:tcursor txn)))
        (when (pagetree:cursor-seek cur pb)
          (loop for k = (pagetree:cursor-key cur)
                while (and k (%bytes-prefix-p k pb))
                do (funcall fn (ascii (subseq k plen)))
                   (unless (pagetree:cursor-next cur) (return))))))))

(defun %dir-prefix (reldir)
  (if (and (plusp (length reldir)) (char= (char reldir (1- (length reldir))) #\/))
      reldir (concatenate 'string reldir "/")))

(defun make-pagetree-backend (store &key (prefix ""))
  "An FS-BACKEND that stores the git store directly in the pagetree STORE, each
   git-dir-relative path as one key (namespaced under PREFIX, so several repos can
   share a store).  Supplies begin-txn/end-txn, so cairn batches a whole
   commit/push into one atomic pagetree transaction."
  (let ((be (make-ptbe :store store :prefix prefix)))
    (make-fs-backend
     :read-bytes   (lambda (rel) (%in-read (txn be) (pagetree:tget txn (%ptbe-key be rel))))
     :read-string  (lambda (rel) (let ((v (%in-read (txn be) (pagetree:tget txn (%ptbe-key be rel)))))
                                   (and v (ascii v))))
     :write-bytes  (lambda (rel bytes)
                     (%in-write (txn be)
                       (pagetree:tput txn (%ptbe-key be rel)
                                      (coerce bytes '(simple-array (unsigned-byte 8) (*))))) t)
     :write-string (lambda (rel text)
                     (%in-write (txn be) (pagetree:tput txn (%ptbe-key be rel) (string->bytes text))) t)
     :exists-p     (lambda (rel) (and (%in-read (txn be) (pagetree:tget txn (%ptbe-key be rel))) t))
     :delete-file  (lambda (rel) (%in-write (txn be) (pagetree:tdel txn (%ptbe-key be rel))) t)
     :dir-exists-p (lambda (rel)
                     (let ((pb (string->bytes (concatenate 'string (ptbe-prefix be) (%dir-prefix rel)))))
                       (%in-read (txn be)
                         (let ((cur (pagetree:tcursor txn)))
                           (and (pagetree:cursor-seek cur pb)
                                (%bytes-prefix-p (pagetree:cursor-key cur) pb))))))
     :list-files   (lambda (rel)
                     (let ((dir (%dir-prefix rel)) (out '()))
                       (%ptbe-scan be dir
                                   (lambda (p) (let ((rest (subseq p (length dir))))
                                                 (unless (find #\/ rest) (push rest out)))))
                       (nreverse out)))
     :walk-files   (lambda (rel)
                     (let ((out '())) (%ptbe-scan be rel (lambda (p) (push p out))) (nreverse out)))
     :begin-txn    (lambda () (setf (ptbe-txn be) (pagetree::begin-txn (ptbe-store be) :write)) t)
     :end-txn      (lambda (commit-p)
                     (let ((txn (ptbe-txn be)))
                       (setf (ptbe-txn be) nil)
                       (when txn (if commit-p (pagetree:commit-txn txn) (pagetree:abort-txn txn))))
                     t))))

(defun open-pagetree-repository (store &key (prefix "") init)
  "Open a (bare) cairn repository whose git store is the pagetree STORE directly,
   under key namespace PREFIX.  With :INIT, first lay down a fresh empty store.
   Returns the repository."
  (let ((repo (make-repository :backend (make-pagetree-backend store :prefix prefix)
                               :git-dir prefix :path prefix)))
    (when init (init-repository repo))
    (setf (repo-format repo) (detect-object-format repo))
    repo))
