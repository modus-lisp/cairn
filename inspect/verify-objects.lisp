;;;; verify-objects.lisp — read every object in a repo and re-hash it.
;;;;
;;;;   sbcl --non-interactive --load inspect/verify-objects.lisp [REPO]
;;;;
;;;; The definitive read-side check: git's ground-truth object list vs cairn's
;;;; read-object + hash-object.  Defaults to /tmp/gitdelta (a packed repo).

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *repo-path* (or (second (member "--" sb-ext:*posix-argv* :test #'string=))
                        "/tmp/gitdelta"))

(let ((repo (cairn:open-repository *repo-path*)) (ok 0) (bad 0))
  (dolist (sha (uiop:split-string
                (uiop:run-program (list "git" "-C" *repo-path* "cat-file"
                                        "--batch-all-objects" "--batch-check=%(objectname)")
                                  :output :string)
                :separator '(#\Newline)))
    (when (plusp (length sha))
      (handler-case
          (multiple-value-bind (type content) (cairn:read-object repo sha)
            (if (string= sha (cairn:hash-object type content)) (incf ok)
                (progn (incf bad) (format t "MISMATCH ~a~%" sha))))
        (error (e) (incf bad) (format t "ERROR ~a: ~a~%" sha e)))))
  (format t "~&verified ~d objects, ~d failures~%" ok bad))
