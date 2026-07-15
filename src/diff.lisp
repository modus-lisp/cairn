;;;; diff.lisp — unified diffs between blobs, the index, and the working tree.
;;;;
;;;; A line-oriented diff: split both sides into lines, find their longest
;;;; common subsequence (a classic DP over line equality), and read off the
;;;; edit script.  Group the edits into hunks with a few lines of surrounding
;;;; context and print them in git's unified format.  `diff` drives it over the
;;;; repo — worktree-vs-index by default, index-vs-HEAD with :cached.

(in-package #:cairn)

(defun lines-of (bytes)
  "Split BYTES into a vector of lines (newline stripped).  Empty input → no lines."
  (when (zerop (length bytes)) (return-from lines-of (make-array 0)))
  (let ((s (ascii bytes)) (out (make-array 16 :adjustable t :fill-pointer 0)) (start 0))
    (loop for pos = (position #\Newline s :start start)
          do (vector-push-extend (subseq s start pos) out)
             (if pos (setf start (1+ pos)) (return)))
    ;; a trailing newline yields a final empty line we drop, matching git's view
    (when (and (plusp (length out))
               (plusp (length s)) (char= (char s (1- (length s))) #\Newline)
               (string= (aref out (1- (fill-pointer out))) ""))
      (decf (fill-pointer out)))
    out))

(defun lcs-script (a b)
  "Edit script turning vector A into vector B: a list of (OP . LINE) with OP in
   :eq / :del / :add, in order."
  (let* ((n (length a)) (m (length b))
         (dp (make-array (list (1+ n) (1+ m)) :element-type 'fixnum :initial-element 0)))
    (loop for i from (1- n) downto 0 do
      (loop for j from (1- m) downto 0 do
        (setf (aref dp i j)
              (if (string= (aref a i) (aref b j))
                  (1+ (aref dp (1+ i) (1+ j)))
                  (max (aref dp (1+ i) j) (aref dp i (1+ j)))))))
    (let ((out '()) (i 0) (j 0))
      (loop while (and (< i n) (< j m)) do
        (cond ((string= (aref a i) (aref b j)) (push (cons :eq (aref a i)) out) (incf i) (incf j))
              ((>= (aref dp (1+ i) j) (aref dp i (1+ j))) (push (cons :del (aref a i)) out) (incf i))
              (t (push (cons :add (aref b j)) out) (incf j))))
      (loop while (< i n) do (push (cons :del (aref a i)) out) (incf i))
      (loop while (< j m) do (push (cons :add (aref b j)) out) (incf j))
      (nreverse out))))

(defstruct (dline (:conc-name dl-)) op text a b)

(defun number-script (script)
  "Annotate each edit with the A/B line numbers it sits at (1-based)."
  (let ((out '()) (ai 1) (bi 1))
    (dolist (cell script (coerce (nreverse out) 'vector))
      (ecase (car cell)
        (:eq  (push (make-dline :op :eq :text (cdr cell) :a ai :b bi) out) (incf ai) (incf bi))
        (:del (push (make-dline :op :del :text (cdr cell) :a ai :b bi) out) (incf ai))
        (:add (push (make-dline :op :add :text (cdr cell) :a ai :b bi) out) (incf bi))))))

(defun unified-hunks (script &optional (context 3))
  "Group SCRIPT into unified-diff hunks: (A-START A-LEN B-START B-LEN LINES)."
  (let* ((v (number-script script)) (n (length v)) (hunks '()) (k 0))
    (flet ((el-changed (d) (not (eq (dl-op d) :eq))))
      (loop while (< k n) do
        (if (not (el-changed (aref v k)))
            (incf k)
            ;; a change starts at k: bracket with CONTEXT lines, merging further
            ;; changes separated by <= 2*CONTEXT equal lines into one hunk
            (let ((start (max 0 (- k context))) (last k) (j (1+ k)))
              (loop
                (let ((nxt (position-if #'el-changed v :start (min j n))))
                  (if (and nxt (<= (- nxt last) (* 2 context)))
                      (setf last nxt j (1+ nxt))
                      (return))))
              (let* ((end (min n (+ last 1 context)))
                     (lines (coerce (subseq v start end) 'list))
                     (a-len (count-if (lambda (d) (member (dl-op d) '(:eq :del))) lines))
                     (b-len (count-if (lambda (d) (member (dl-op d) '(:eq :add))) lines))
                     ;; a zero-length side numbers from the line *before* the range
                     (a-start (if (zerop a-len) (1- (dl-a (first lines))) (dl-a (first lines))))
                     (b-start (if (zerop b-len) (1- (dl-b (first lines))) (dl-b (first lines)))))
                (push (list a-start a-len b-start b-len lines) hunks)
                (setf k end)))))
      (nreverse hunks))))

(defun format-unified (a-bytes b-bytes path &key (context 3) (stream *standard-output*)
                                                 (a-prefix "a/") (b-prefix "b/"))
  "Print a git-style unified diff of A-BYTES → B-BYTES for PATH.  Returns T if
   there were any differences."
  (let ((hunks (unified-hunks (lcs-script (lines-of a-bytes) (lines-of b-bytes)) context)))
    (when hunks
      (format stream "diff --git ~a~a ~a~a~%" a-prefix path b-prefix path)
      (format stream "--- ~a~%" (if (zerop (length a-bytes)) "/dev/null" (format nil "~a~a" a-prefix path)))
      (format stream "+++ ~a~%" (if (zerop (length b-bytes)) "/dev/null" (format nil "~a~a" b-prefix path)))
      (flet ((range (start len) (if (= len 1) (format nil "~d" start) (format nil "~d,~d" start len))))
        (dolist (h hunks)
          (destructuring-bind (as al bs bl lines) h
            (format stream "@@ -~a +~a @@~%" (range as al) (range bs bl))
            (dolist (d lines)
              (format stream "~a~a~%" (ecase (dl-op d) (:eq " ") (:del "-") (:add "+")) (dl-text d)))))))
    (and hunks t)))

(defun blob-or-empty (repo sha)
  (if sha (object-data repo sha) (make-array 0 :element-type '(unsigned-byte 8))))

(defun worktree-bytes (repo path)
  (let ((abs (worktree-path repo path)))
    (if (probe-file abs) (slurp-bytes abs) (make-array 0 :element-type '(unsigned-byte 8)))))

(defun diff (repo &key cached (stream *standard-output*))
  "Print unified diffs.  Default: working tree vs index (unstaged changes).
   :cached t: index vs HEAD (staged changes)."
  (with-oid (repo)
  (let ((index-map (make-hash-table :test 'equal)))
    (loop for e across (read-index (repo-git-dir repo))
          do (setf (gethash (ie-path e) index-map) (ie-sha e)))
    (if cached
        (let ((head (head-tree-map repo)) (seen (make-hash-table :test 'equal)))
          (maphash (lambda (path sha)
                     (setf (gethash path seen) t)
                     (format-unified (blob-or-empty repo (gethash path head))
                                     (blob-or-empty repo sha) path :stream stream))
                   index-map)
          (maphash (lambda (path sha)
                     (unless (gethash path seen)
                       (format-unified (blob-or-empty repo sha)
                                       (make-array 0 :element-type '(unsigned-byte 8))
                                       path :stream stream)))
                   head))
        (maphash (lambda (path sha)
                   (format-unified (blob-or-empty repo sha) (worktree-bytes repo path)
                                   path :stream stream))
                 index-map)))))
