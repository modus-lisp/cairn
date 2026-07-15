(require :asdf)
(dolist (f '("packages" "sha1" "inflate" "objects" "refs" "pack" "repository" "plumbing"))
  (load (compile-file (format nil "/home/claude/cairn/src/~a.lisp" f) :verbose nil :print nil)))
(in-package :cairn)
(let ((repo (open-repository "/tmp/gitpack")) (ok 0) (bad 0))
  (with-open-file (s "/tmp/allshas.txt")
    (loop for sha = (read-line s nil nil) while sha do
      (handler-case
          (multiple-value-bind (type content) (read-object repo sha)
            (if (string= sha (hash-object type content)) (incf ok)
                (progn (incf bad) (format t "MISMATCH ~a~%" sha))))
        (error (e) (incf bad) (format t "ERROR ~a: ~a~%" sha e)))))
  (format t "~&verified ~d objects, ~d failures~%" ok bad))
