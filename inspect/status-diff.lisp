;;;; status-diff.lisp — cairn status/diff next to real git's, on one repo.
;;;;
;;;;   sbcl --non-interactive --load inspect/status-diff.lisp
;;;;
;;;; Builds a repo with a staged edit + a staged new file + an unstaged edit + an
;;;; untracked file, then prints git's status/diff and cairn's for comparison.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *r* "/tmp/cairn-statusdiff")
(uiop:delete-directory-tree (uiop:ensure-directory-pathname *r*)
                            :validate t :if-does-not-exist :ignore)
(flet ((git (&rest args) (uiop:run-program (list* "git" "-C" *r* args)
                                           :output :string :ignore-error-status t))
       (put (rel text) (uiop:with-output-file (s (format nil "~a/~a" *r* rel) :if-exists :supersede)
                         (write-string text s))))
  (ensure-directories-exist (format nil "~a/" *r*))
  (git "init" "-q")
  (put "a.txt" (format nil "line one~%line two~%line three~%line four~%line five~%"))
  (git "add" "a.txt") (git "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qm" "init")
  (put "a.txt" (format nil "line one~%line two~%line THREE changed~%line four~%line five~%"))
  (put "new.txt" (format nil "fresh staged file~%"))
  (git "add" "a.txt" "new.txt")
  (put "a.txt" (format nil "line one~%line two~%line THREE changed~%line four~%line FIVE also~%"))
  (put "untracked.txt" (format nil "not tracked~%"))
  (let ((repo (cairn:open-repository *r*)))
    (format t "======== git status ========~%~a~%" (git "-c" "color.ui=never" "status" "-s"))
    (format t "======== cairn status ========~%")   (cairn:print-status repo)
    (format t "~%======== git diff ========~%~a" (git "diff"))
    (format t "======== cairn diff ========~%")      (cairn:diff repo)
    (format t "~%======== git diff --cached ========~%~a" (git "diff" "--cached"))
    (format t "======== cairn diff --cached ========~%") (cairn:diff repo :cached t)))
