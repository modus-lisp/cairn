;;;; merge.lisp — three-way merge, checked against real git.
;;;;
;;;;   sbcl --non-interactive --load inspect/merge.lisp
;;;;
;;;; Builds two divergent branches and merges them three ways: real git, cairn
;;;; (default resolver = conflict markers), and cairn with a custom resolver.
;;;; A clean merge must produce a tree byte-identical to git's; a conflict must
;;;; produce identical markers + a git-readable unmerged index.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defun sh (dir &rest args)
  (string-trim '(#\Newline)
    (uiop:run-program (list* "git" "-C" dir args) :output :string :ignore-error-status t)))

(defun fresh-copy (src dst)
  "Copy SRC to DST, replacing any existing DST."
  (uiop:delete-directory-tree (uiop:ensure-directory-pathname dst) :validate t :if-does-not-exist :ignore)
  (uiop:run-program (list "cp" "-r" src dst)))

(defun build (root file-ours file-theirs)
  "A repo at ROOT with main (ours) and feature (theirs) diverging from a base."
  (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist (format nil "~a/" root))
  (flet ((put (rel text) (uiop:with-output-file (s (format nil "~a/~a" root rel) :if-exists :supersede)
                           (write-string text s)))
         (g (&rest a) (apply #'sh root a)))
    (g "init" "-q" "-b" "main" ".")
    (put "f.txt" (format nil "title~%shared line~%footer~%"))
    (g "add" "-A") (g "-c" "user.name=b" "-c" "user.email=b@b" "commit" "-qm" "base")
    (g "branch" "feature")
    (put "f.txt" file-ours)
    (g "add" "-A") (g "-c" "user.name=o" "-c" "user.email=o@o" "commit" "-qm" "ours")
    (g "checkout" "-q" "feature")
    (put "f.txt" file-theirs)
    (g "add" "-A") (g "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qm" "theirs")
    (g "checkout" "-q" "main")))

;; --- clean merge: edits in different regions --------------------------------
(build "/tmp/mi-git"
       (format nil "OURS~%shared line~%footer~%")
       (format nil "title~%shared line~%THEIRS~%"))
(fresh-copy "/tmp/mi-git" "/tmp/mi-cairn")
(sh "/tmp/mi-git" "-c" "user.name=m" "-c" "user.email=m@m" "merge" "--no-edit" "feature")
(cairn:merge (cairn:open-repository "/tmp/mi-cairn") "refs/heads/feature"
             :theirs-label "feature" :author "m <m@m>")
(format t "~&CLEAN MERGE~%  git tree:   ~a~%  cairn tree: ~a~%  identical: ~a~%"
        (sh "/tmp/mi-git" "rev-parse" "HEAD^{tree}")
        (sh "/tmp/mi-cairn" "rev-parse" "HEAD^{tree}")
        (string= (sh "/tmp/mi-git" "rev-parse" "HEAD^{tree}")
                 (sh "/tmp/mi-cairn" "rev-parse" "HEAD^{tree}")))
(format t "  cairn parents: ~a  |  git parents: ~a~%"
        (sh "/tmp/mi-cairn" "log" "-1" "--format=%p") (sh "/tmp/mi-git" "log" "-1" "--format=%p"))

;; --- conflict merge: both edit the same line --------------------------------
(build "/tmp/mi-git2"
       (format nil "title~%OURS version~%footer~%")
       (format nil "title~%THEIRS version~%footer~%"))
(fresh-copy "/tmp/mi-git2" "/tmp/mi-cairn2")
(sh "/tmp/mi-git2" "-c" "user.name=m" "-c" "user.email=m@m" "merge" "--no-edit" "feature")
(cairn:merge (cairn:open-repository "/tmp/mi-cairn2") "refs/heads/feature" :theirs-label "feature")
(format t "~%CONFLICT MERGE~%  markers identical to git: ~a~%  cairn git-status: ~a  (git: ~a)~%"
        (string= (uiop:read-file-string "/tmp/mi-git2/f.txt")
                 (uiop:read-file-string "/tmp/mi-cairn2/f.txt"))
        (sh "/tmp/mi-cairn2" "status" "-s") (sh "/tmp/mi-git2" "status" "-s"))
(format t "  unmerged stages git reads from cairn's index: ~a~%"
        (length (uiop:split-string (sh "/tmp/mi-cairn2" "ls-files" "-u") :separator '(#\Newline))))

;; --- recursive: a criss-cross history with TWO merge bases ------------------
(let ((root "/tmp/mi-cc"))
  (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist (format nil "~a/" root))
  (flet ((put (text) (uiop:with-output-file (s (format nil "~a/f.txt" root) :if-exists :supersede)
                       (write-string text s)))
         (g (&rest a) (apply #'sh root a)))
    (g "init" "-q" "-b" "main" ".")
    (put (format nil "1~%2~%3~%4~%5~%")) (g "add" "-A") (g "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qm" "O")
    (g "branch" "feature")
    (put (format nil "1~%2~%3~%4~%A5~%")) (g "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qam" "A")
    (let ((a (sh root "rev-parse" "HEAD")))
      (g "checkout" "-q" "feature")
      (put (format nil "B1~%2~%3~%4~%5~%")) (g "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qam" "B")
      (g "checkout" "-q" "main")    (g "-c" "user.name=t" "-c" "user.email=t@t" "merge" "--no-edit" "-q" "feature")
      (g "checkout" "-q" "feature") (g "-c" "user.name=t" "-c" "user.email=t@t" "merge" "--no-edit" "-q" a)
      (g "checkout" "-q" "main")
      (put (format nil "B1~%M2~%3~%4~%A5~%")) (g "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qam" "M2")
      (g "checkout" "-q" "feature")
      (put (format nil "B1~%2~%3~%N4~%A5~%")) (g "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qam" "N2")
      (g "checkout" "-q" "main")))
  (fresh-copy root "/tmp/mi-cc-git")
  (sh "/tmp/mi-cc-git" "-c" "user.name=m" "-c" "user.email=m@m" "merge" "--no-edit" "feature")
  (let ((repo (cairn:open-repository root)))
    (format t "~%RECURSIVE MERGE (criss-cross, 2 bases)~%  merge bases found by cairn: ~a~%"
            (length (cairn:all-merge-bases repo (cairn:head-commit repo)
                                                (cairn::rev-parse repo "refs/heads/feature"))))
    (cairn:merge repo "refs/heads/feature" :theirs-label "feature" :author "m <m@m>")
    (format t "  clean merge tree == git's: ~a~%"
            (string= (sh root "rev-parse" "HEAD^{tree}") (sh "/tmp/mi-cc-git" "rev-parse" "HEAD^{tree}")))))
