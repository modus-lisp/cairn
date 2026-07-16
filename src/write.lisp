;;;; write.lisp — writing objects into the loose object store.
;;;;
;;;; The mirror of read-object: take a type and content, prepend git's
;;;; "<type> <size>\0" header, and the SHA-1 of that is the object id.  Store it
;;;; zlib-compressed (salza2) at objects/xx/yyyy…, unless it is already present
;;;; (objects are immutable and content-addressed, so a matching id is a no-op).

(in-package #:cairn)

(defun write-object (repo type content)
  "Write an object of TYPE with CONTENT bytes to REPO's loose store.  Returns
   its SHA-1 hex id.  A no-op if the object already exists."
  (with-oid (repo)
  (let* ((body (concatenate 'u8v (object-header type content) content))
         (sha (oid-hex body))
         (rel (loose-object-relpath sha)))
    (unless (fs-exists-p repo rel)
      (fs-write-bytes repo rel (zlib-compress body)))
    sha)))

(defun write-blob-from-file (repo relpath)
  "Read the working-tree file (or symlink) at repo-relative RELPATH, write it as a
   blob, and return (values SHA TREE-MODE) — TREE-MODE is git's octal mode string."
  (let ((st (wt-lstat repo relpath)))
    (if (eq (wts-type st) :symlink)
        (values (write-object repo :blob (string->bytes (wt-read-symlink repo relpath))) "120000")
        (values (write-object repo :blob (wt-read-file repo relpath))
                (if (logtest (wts-mode st) #o111) "100755" "100644")))))
