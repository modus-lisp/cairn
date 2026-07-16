;;;; objects.lisp — the git object model (blob, tree, commit, tag).
;;;;
;;;; A git object is stored as zlib("<type> <size>\0<content>") and addressed by
;;;; the SHA-1 of that whole byte string.  This file parses the four object types
;;;; once the raw bytes are in hand (repository.lisp does the loose/pack lookup).

(in-package #:cairn)

(defun slurp-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s)
      b)))

(defun slurp-string (path)
  (with-open-file (s path :element-type 'character :if-does-not-exist nil)
    (when s
      (let ((str (make-string (file-length s))))
        (subseq str 0 (read-sequence str s))))))

(defun ascii (bytes) (map 'string #'code-char bytes))

(defun split-lines (s)
  (loop for start = 0 then (1+ pos)
        for pos = (position #\Newline s :start start)
        collect (subseq s start pos)
        while pos))

;;; ---- generic object ---------------------------------------------------------
(defun parse-object (raw)
  "RAW is the full decompressed object bytes.  Returns (values TYPE-KEYWORD
   CONTENT-BYTES) where TYPE is :commit / :tree / :blob / :tag."
  (let* ((nul (position 0 raw))
         (header (ascii (subseq raw 0 nul)))
         (space (position #\Space header)))
    (values (intern (string-upcase (subseq header 0 space)) :keyword)
            (subseq raw (1+ nul)))))

(defun object-header (type content)
  "The '<type> <size>\\0' prefix git prepends before hashing/storing CONTENT."
  (string->bytes (format nil "~(~a~) ~d~c" type (length content) (code-char 0))))

(defun string->bytes (s) (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defun hash-object (type content)
  "The git object id (hex) of an object of TYPE with CONTENT bytes, under the
   active object format (*oid*).  Streams the header then the content into the
   hash, so nothing is copied into a combined buffer."
  (oid-hex-parts (object-header type content) content))

;;; ---- commit -----------------------------------------------------------------
(defstruct commit tree parents author committer message)

(defun parse-commit (content)
  (let* ((text (ascii content))
         (blank (search (coerce '(#\Newline #\Newline) 'string) text))
         (headers (subseq text 0 blank))
         (message (if blank (subseq text (+ blank 2)) ""))
         (tree nil) (parents '()) (author nil) (committer nil))
    (dolist (line (split-lines headers))
      (let ((sp (position #\Space line)))
        (when sp
          (let ((key (subseq line 0 sp)) (val (subseq line (1+ sp))))
            (cond ((string= key "tree") (setf tree val))
                  ((string= key "parent") (push val parents))
                  ((string= key "author") (setf author val))
                  ((string= key "committer") (setf committer val)))))))
    (make-commit :tree tree :parents (nreverse parents)
                 :author author :committer committer :message message)))

;;; ---- tree -------------------------------------------------------------------
(defstruct tree-entry mode name sha)

(defun parse-tree (content)
  "Return a list of TREE-ENTRY (mode string, name string, sha hex)."
  (let ((entries '()) (i 0) (n (length content)))
    (loop while (< i n) do
      (let* ((sp (position 32 content :start i))
             (mode (ascii (subseq content i sp)))
             (nul (position 0 content :start sp))
             (name (ascii (subseq content (1+ sp) nul)))
             (end (+ nul 1 (oid-nbytes)))
             (sha (bytes->hex (subseq content (1+ nul) end))))
        (push (make-tree-entry :mode mode :name name :sha sha) entries)
        (setf i end)))
    (nreverse entries)))

(defun tree-entries (content) (parse-tree content))
