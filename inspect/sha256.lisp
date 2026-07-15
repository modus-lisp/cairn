;;;; sha256.lisp — read and write a SHA-256 repository, checked against git.
;;;;
;;;;   sbcl --non-interactive --load inspect/sha256.lisp
;;;;
;;;; git can address objects by SHA-256 (git init --object-format=sha256).  cairn
;;;; detects the format from .git/config and binds *oid* accordingly, so every
;;;; width (object ids, tree entries, pack/index) follows.  This builds such a
;;;; repo, has cairn re-hash every object and write a commit, and asks real git
;;;; to validate — the committed SHA must equal git rev-parse HEAD.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *r* "/tmp/cairn-sha256")

(flet ((sh (&rest a) (uiop:run-program (list* "git" "-C" *r* a)
                                       :output :string :error-output :string :ignore-error-status t)))
  (uiop:delete-directory-tree (uiop:ensure-directory-pathname *r*) :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist (format nil "~a/" *r*))
  (sh "init" "-q" "--object-format=sha256")
  (uiop:with-output-file (s (format nil "~a/a.txt" *r*)) (write-line "sha256 content" s))
  (ensure-directories-exist (format nil "~a/sub/" *r*))
  (uiop:with-output-file (s (format nil "~a/sub/b.txt" *r*)) (write-line "nested" s))
  (sh "add" "-A") (sh "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qm" "c0")

  (let ((repo (cairn:open-repository *r*)))
    (format t "~&object format cairn detected: ~a~%" (cairn::repo-format repo))
    (format t "HEAD is ~d hex chars~%" (length (cairn:head-commit repo)))
    ;; re-hash every object under the repo's format
    (let ((ok 0) (bad 0))
      (cairn::with-oid (repo)
        (dolist (sha (uiop:split-string (sh "cat-file" "--batch-all-objects" "--batch-check=%(objectname)")
                                        :separator '(#\Newline)))
          (when (plusp (length sha))
            (multiple-value-bind (ty c) (cairn:read-object repo sha)
              (if (string= sha (cairn:hash-object ty c)) (incf ok) (incf bad))))))
      (format t "re-hash (loose): ~d ok, ~d bad~%" ok bad))
    ;; write a commit and let git validate it
    (uiop:with-output-file (s (format nil "~a/a.txt" *r*) :if-exists :append)
      (write-line "committed by cairn" s))
    (cairn:add repo "a.txt")
    (let ((sha (cairn:commit repo :message "cairn: sha256 commit" :author "me <me@me>")))
      (format t "cairn commit sha:     ~a~%" sha)
      (format t "git rev-parse HEAD:   ~a  [match: ~a]~%"
              (string-trim '(#\Newline) (sh "rev-parse" "HEAD"))
              (string= sha (string-trim '(#\Newline) (sh "rev-parse" "HEAD"))))
      (format t "git fsck:   ~a~%" (if (string= "" (sh "fsck" "--full")) "clean" "errors"))
      (format t "git status: ~a~%" (if (string= "" (sh "status" "-s")) "clean" "dirty")))
    ;; and read it back from a pack
    (sh "repack" "-adq")
    (let ((ok 0) (bad 0))
      (cairn::with-oid (repo)
        (setf (cairn::repo-packs repo) :unloaded)
        (dolist (sha (uiop:split-string (sh "cat-file" "--batch-all-objects" "--batch-check=%(objectname)")
                                        :separator '(#\Newline)))
          (when (plusp (length sha))
            (multiple-value-bind (ty c) (cairn:read-object repo sha)
              (if (string= sha (cairn:hash-object ty c)) (incf ok) (incf bad))))))
      (format t "re-hash (packed):~d ok, ~d bad~%" ok bad))))
