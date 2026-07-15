;;;; plumbing.lisp — read-side porcelain: cat-file, log, ls-tree, rev-parse.

(in-package #:cairn)

(defun rev-parse (repo rev)
  "Resolve REV (a ref name, HEAD, or a full 40-hex sha) to a sha."
  (cond ((and (= (length rev) 40) (every (lambda (c) (digit-char-p c 16)) rev)) rev)
        ((resolve-ref repo rev))
        ((resolve-ref repo (concatenate 'string "refs/heads/" rev)))
        ((resolve-ref repo (concatenate 'string "refs/tags/" rev)))
        (t (error "cairn: unknown revision ~a" rev))))

(defun cat-file (repo rev)
  "Return (values TYPE CONTENT-BYTES) for the object REV names."
  (read-object repo (rev-parse repo rev)))

(defun cat-file-string (repo rev)
  "cat-file, with the content decoded as a string (git cat-file -p style)."
  (multiple-value-bind (type content) (cat-file repo rev)
    (values (ascii content) type)))

(defun ls-tree (repo rev)
  "List the tree of REV (a tree sha, or a commit whose tree is used)."
  (let ((sha (rev-parse repo rev)))
    (multiple-value-bind (type content) (read-object repo sha)
      (parse-tree (ecase type
                    (:tree content)
                    (:commit (nth-value 1 (read-object repo (commit-tree (parse-commit content))))))))))

(defun log-commits (repo &key (start "HEAD") limit)
  "Walk the first-parent ancestry from START; return a list of (SHA . COMMIT)."
  (let ((result '()) (sha (rev-parse repo start)) (count 0))
    (loop while (and sha (or (null limit) (< count limit))) do
      (multiple-value-bind (type content) (read-object repo sha)
        (declare (ignore type))
        (let ((commit (parse-commit content)))
          (push (cons sha commit) result)
          (incf count)
          (setf sha (first (commit-parents commit))))))
    (nreverse result)))

(defun commit-summary (commit)
  "The first line of a commit message."
  (let ((msg (commit-message commit)))
    (subseq msg 0 (or (position #\Newline msg) (length msg)))))
