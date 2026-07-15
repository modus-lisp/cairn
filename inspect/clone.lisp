;;;; clone.lisp — end-to-end transport check: clone a real repo over HTTPS.
;;;;
;;;;   sbcl --non-interactive --load inspect/clone.lisp
;;;;
;;;; Clones modus-lisp/natrium over the smart HTTP protocol (cairn -> seal TLS
;;;; -> natrium crypto), then re-verifies every object re-hashes to its id.  For
;;;; the definitive external check, hand the result to real git afterwards:
;;;;   git -C /tmp/cairn-clone fsck --full --strict
;;;;   git -C /tmp/cairn-clone verify-pack -v .git/objects/pack/*.idx

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *url* "https://github.com/modus-lisp/natrium")
(defvar *dest* "/tmp/cairn-clone")

(uiop:delete-directory-tree (uiop:ensure-directory-pathname *dest*)
                            :validate t :if-does-not-exist :ignore)

(let ((repo (cairn:clone *url* *dest*))
      (ok 0) (bad 0))
  (format t "~&HEAD: ~a~%" (cairn:head-commit repo))
  ;; re-hash every object we stored
  (dolist (line (uiop:split-string
                 (uiop:run-program (list "git" "-C" *dest* "cat-file"
                                         "--batch-all-objects" "--batch-check=%(objectname)")
                                   :output :string)
                 :separator '(#\Newline)))
    (when (plusp (length line))
      (handler-case
          (multiple-value-bind (type content) (cairn:read-object repo line)
            (if (string= line (cairn:hash-object type content)) (incf ok)
                (progn (incf bad) (format t "MISMATCH ~a~%" line))))
        (error (e) (incf bad) (format t "ERROR ~a: ~a~%" line e)))))
  (format t "~&verified ~d objects, ~d failures~%" ok bad))
