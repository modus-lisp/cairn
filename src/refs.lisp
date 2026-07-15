;;;; refs.lisp — references (HEAD, branches, tags, packed-refs).

(in-package #:cairn)

(defun packed-ref (repo ref)
  "Look REF up in .git/packed-refs; return its sha or NIL."
  (let ((path (merge-pathnames "packed-refs" (repo-git-dir repo))))
    (let ((text (slurp-string path)))
      (when text
        (dolist (line (split-lines text))
          (unless (or (zerop (length line)) (member (char line 0) '(#\# #\^)))
            (let ((sp (position #\Space line)))
              (when (and sp (string= (subseq line (1+ sp)) ref))
                (return-from packed-ref (subseq line 0 sp))))))))))

(defun resolve-ref (repo ref)
  "Resolve REF (\"HEAD\", \"refs/heads/master\", …) to a 40-char sha, following
   symbolic (\"ref: …\") references and consulting packed-refs.  NIL if unknown."
  (let* ((path (merge-pathnames ref (repo-git-dir repo)))
         (content (and (probe-file path)
                       (string-trim '(#\Newline #\Space #\Return #\Tab) (slurp-string path)))))
    (cond
      ((null content) (packed-ref repo ref))
      ((and (>= (length content) 5) (string= (subseq content 0 5) "ref: "))
       (resolve-ref repo (subseq content 5)))
      (t content))))

(defun ref-target (repo ref)
  "The immediate target of REF: (values KIND VALUE) — either (:ref \"refs/…\") for
   a symbolic ref or (:sha \"<hex>\") for a direct one."
  (let* ((path (merge-pathnames ref (repo-git-dir repo)))
         (content (and (probe-file path)
                       (string-trim '(#\Newline #\Space #\Return) (slurp-string path)))))
    (cond ((null content) (values :sha (packed-ref repo ref)))
          ((and (>= (length content) 5) (string= (subseq content 0 5) "ref: "))
           (values :ref (subseq content 5)))
          (t (values :sha content)))))

(defun list-refs (repo)
  "An alist of (refname . sha) for everything under refs/ (loose and packed)."
  (let ((result '()) (refs-dir (merge-pathnames "refs/" (repo-git-dir repo)))
        (git-dir (repo-git-dir repo)))
    (labels ((walk (dir)
               (dolist (p (append (uiop:directory-files dir) (uiop:subdirectories dir)))
                 (if (uiop:directory-exists-p p)
                     (walk p)
                     (let ((name (enough-namestring p git-dir)))
                       (push (cons name (resolve-ref repo name)) result))))))
      (when (uiop:directory-exists-p refs-dir) (walk refs-dir)))
    ;; packed-refs entries not shadowed by a loose ref
    (let ((text (slurp-string (merge-pathnames "packed-refs" git-dir))))
      (when text
        (dolist (line (split-lines text))
          (unless (or (zerop (length line)) (member (char line 0) '(#\# #\^)))
            (let ((sp (position #\Space line)))
              (when sp
                (let ((name (subseq line (1+ sp))))
                  (unless (assoc name result :test #'string=)
                    (push (cons name (subseq line 0 sp)) result)))))))))
    (nreverse result)))

(defun head-commit (repo) (resolve-ref repo "HEAD"))
