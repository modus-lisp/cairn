;;;; commit.lisp — end-to-end write side: clone, edit, add, commit, ask git.
;;;;
;;;;   sbcl --non-interactive --load inspect/commit.lisp
;;;;
;;;; Clones natrium, edits a file + adds a new one in a new subdir, makes a
;;;; commit with cairn, then has REAL git judge it: fsck clean, our commit SHA ==
;;;; git rev-parse HEAD (proves byte-exact serialization), status clean.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *dest* (uiop:ensure-directory-pathname "/tmp/cairn-commit"))
(uiop:delete-directory-tree *dest* :validate t :if-does-not-exist :ignore)

(let* ((repo (cairn:clone "https://github.com/modus-lisp/natrium" *dest*))
       (old (cairn:head-commit repo)))
  (with-open-file (s (merge-pathnames "README.md" *dest*) :direction :output :if-exists :append)
    (write-line "" s) (write-line "Edited by cairn." s))
  (ensure-directories-exist (merge-pathnames "notes/" *dest*))
  (with-open-file (s (merge-pathnames "notes/hello.txt" *dest*)
                     :direction :output :if-exists :supersede)
    (write-line "new file, new subdir" s))
  (cairn:add repo "README.md" "notes/hello.txt")
  (let ((sha (cairn:commit repo :message "cairn: self-authored commit"
                                :author "ynniv <anthropic@ynniv.com>")))
    (flet ((git (&rest args)
             (string-trim '(#\Newline)
               (uiop:run-program (list* "git" "-C" (namestring *dest*) args) :output :string
                                 :ignore-error-status t))))
      (format t "~&parent (old HEAD): ~a~%cairn commit:      ~a~%" old sha)
      (format t "git rev-parse HEAD: ~a  [match: ~a]~%" (git "rev-parse" "HEAD")
              (string= sha (git "rev-parse" "HEAD")))
      (format t "git fsck:           ~a~%"
              (let ((o (git "fsck" "--full" "--strict"))) (if (string= o "") "clean" o)))
      (format t "git status:         ~a~%"
              (if (string= (git "status" "--short") "") "clean" "DIRTY"))
      (format t "git log -2:~%~a~%" (git "log" "--oneline" "-2")))))
