;;;; delta.lisp — delta compression: size vs git, and git can read our packs.
;;;;
;;;;   sbcl --non-interactive --load inspect/delta.lisp
;;;;
;;;; Builds a repo whose big file is edited across many commits (near-duplicate
;;;; blobs), then compares cairn's undeltified vs deltified pack, checks the
;;;; delta encoder round-trips through our own reader, and — the real test — has
;;;; real git verify-pack cairn's deltified pack (resolving every delta).

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :cairn)))

(defvar *repo* "/tmp/delta-demo")

(flet ((sh (dir &rest a) (uiop:run-program (list* "git" "-C" dir a)
                                           :output :string :error-output :string :ignore-error-status t)))
  (uiop:delete-directory-tree (uiop:ensure-directory-pathname *repo*) :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist (format nil "~a/" *repo*))
  (sh *repo* "init" "-q")
  (labels ((line (i seed) (format nil "line ~d: ~d~%" i (mod (* (+ i seed) 2654435761) 100000)))
           (write-data (seed n)
             (uiop:with-output-file (s (format nil "~a/data.txt" *repo*) :if-exists :supersede)
               (dotimes (i 300) (write-string (line i (if (zerop (mod i 37)) (+ seed n) 0)) s)))))
    (write-data 0 0) (sh *repo* "add" "-A") (sh *repo* "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qm" "c0")
    (dotimes (n 20)
      (write-data 1 (1+ n))
      (sh *repo* "add" "-A") (sh *repo* "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-qm" (format nil "c~d" (1+ n)))))
  (sh *repo* "repack" "-adq")
  (let* ((repo (cairn:open-repository *repo*))
         (shas (let (o) (maphash (lambda (k v) (declare (ignore v)) (push k o))
                                 (cairn:reachable-objects repo (cairn:head-commit repo))) o))
         (undelta (cairn:write-packfile repo shas :deltify nil))
         (delta (cairn:write-packfile repo shas))
         (gitpack (parse-integer (string-trim '(#\Newline #\Space)
                    (uiop:run-program (list "sh" "-c" (format nil "du -b ~a/.git/objects/pack/*.pack | cut -f1" *repo*))
                                      :output :string)))))
    (format t "~&objects:            ~d~%" (length shas))
    (format t "cairn undeltified:  ~d bytes~%" (length undelta))
    (format t "cairn deltified:    ~d bytes  (~,1fx smaller)~%" (length delta)
            (/ (length undelta) (length delta) 1.0))
    (format t "git's own pack:     ~d bytes  (cairn is ~d% of git's size)~%" gitpack
            (round (* 100 (/ (length delta) gitpack))))
    ;; round-trip through our own reader
    (cairn::write-bytes "/tmp/delta-demo.pack" delta)
    (uiop:delete-directory-tree (uiop:ensure-directory-pathname "/tmp/delta-rt/") :validate t :if-does-not-exist :ignore)
    (cairn:index-pack delta "/tmp/delta-rt/objects/pack/")
    (let ((r2 (cairn::make-repository :path #p"/tmp/delta-rt/" :git-dir #p"/tmp/delta-rt/")) (ok 0))
      (dolist (sha shas)
        (multiple-value-bind (ty c) (cairn:read-object r2 sha)
          (when (string= sha (cairn:hash-object ty c)) (incf ok))))
      (format t "our reader round-trip: ~d/~d objects verify~%" ok (length shas)))
    ;; real git verifies the deltified pack
    (uiop:run-program (list "git" "index-pack" "-o" "/tmp/delta-demo.idx" "/tmp/delta-demo.pack")
                      :ignore-error-status t :output nil :error-output nil)
    (let ((v (uiop:run-program (list "git" "verify-pack" "-v" "/tmp/delta-demo.idx")
                               :output :string :ignore-error-status t)))
      (format t "git verify-pack: ~a~%"
              (if (search "chain length" v) "OK (deltas resolved by git)" "unexpected"))
      (format t "~a~%" (string-trim '(#\Newline) (car (last (uiop:split-string v :separator '(#\Newline)) 2)))))))
