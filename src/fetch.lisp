;;;; fetch.lisp — incremental fetch and fast-forward pull.
;;;;
;;;; Clone starts from nothing; fetch updates a repo we already have.  The wire
;;;; difference is negotiation: we tell the server the commits we already hold
;;;; (`have` lines) so it sends only what's new — often a *thin* pack, whose
;;;; deltas lean on objects already in our store (index-pack resolves those
;;;; against the repo).  Fetch advances the remote-tracking refs
;;;; (refs/remotes/origin/*); pull then fast-forwards the current branch and
;;;; checks it out.  Works over both transports — HTTP and SSH.

(in-package #:cairn)

(defun ssh-url-p (url)
  (or (and (>= (length url) 6) (string= (subseq url 0 6) "ssh://"))
      (and (find #\@ url) (not (search "://" url)))))

(defun local-haves (repo &optional (limit 256))
  "Commit SHAs we already hold, newest-first, for `have` negotiation (capped)."
  (let ((haves '()) (seen (make-hash-table :test 'equal)) (queue '()) (n 0))
    (loop for (nil . sha) in (list-refs repo) do (push sha queue))
    (let ((h (ignore-errors (head-commit repo)))) (when h (push h queue)))
    (loop while (and queue (< n limit)) do
      (let ((sha (pop queue)))
        (when (and sha (not (gethash sha seen)))
          (setf (gethash sha seen) t)
          (handler-case
              (let ((c (parse-commit (object-data repo sha))))    ; commits only
                (push sha haves) (incf n)
                (dolist (p (commit-parents c)) (push p queue)))
            (error () nil)))))
    (nreverse haves)))

(defun wants-and-haves (repo remote-refs)
  "The SHAs to ask for (advertised refs we lack) and the SHAs we can offer."
  (values (remove-duplicates
           (loop for (name . sha) in remote-refs
                 unless (or (string= name "HEAD") (peeled-ref-p name) (have-object-p repo sha))
                   collect sha)
           :test #'string=)
          (local-haves repo)))

(defun read-ref-file (repo refname)
  (let ((p (merge-pathnames refname (repo-git-dir repo))))
    (when (probe-file p) (string-trim '(#\Newline #\Space #\Return) (slurp-string p)))))

(defun remote-url (repo &optional (name "origin"))
  "The url of remote NAME from .git/config, or NIL."
  (let ((cfg (slurp-string (merge-pathnames "config" (repo-git-dir repo))))
        (marker (format nil "[remote \"~a\"]" name)))
    (when cfg
      (let ((start (search marker cfg)))
        (when start
          (let* ((body (subseq cfg (+ start (length marker))))
                 (body (subseq body 0 (search "[" body)))
                 (upos (search "url" body)))
            (when upos
              (let ((eq (position #\= body :start upos)))
                (string-trim '(#\Space #\Tab)
                             (subseq body (1+ eq) (position #\Newline body :start eq)))))))))))

;;; ---- transport ---------------------------------------------------------------

(defun fetch-http (repo url)
  (let ((base (smart-http-base url)))
    (multiple-value-bind (remote-refs caps head-target) (discover-refs base)
      (declare (ignore caps))
      (multiple-value-bind (wants haves) (wants-and-haves repo remote-refs)
        (values remote-refs head-target
                (when wants (fetch-pack base wants haves)))))))

(defun fetch-ssh (repo url identity)
  (let ((target (parse-ssh-url url)))
    (multiple-value-bind (conn chan) (ssh-git-channel target "git-upload-pack" identity)
      (unwind-protect
           (multiple-value-bind (remote-refs caps head-target) (read-ref-advert chan)
             (declare (ignore caps))
             (multiple-value-bind (wants haves) (wants-and-haves repo remote-refs)
               (if (null wants)
                   (values remote-refs head-target nil)
                   (progn
                     (send-wants chan wants haves)
                     (let* ((resp (conch:chan-read-all chan))
                            (start (search #(#x50 #x41 #x43 #x4b) resp)))
                       (unless start (error "cairn: no packfile in fetch response"))
                       (values remote-refs head-target (subseq resp start)))))))
        (ignore-errors (conch:chan-close chan))
        (ignore-errors (conch:disconnect conn))))))

(defun update-remote-refs (repo remote-refs &optional (remote "origin"))
  "Point refs/remotes/REMOTE/* at the advertised branch tips; return (branch . sha)."
  (loop for (name . sha) in remote-refs
        when (and (> (length name) 11) (string= (subseq name 0 11) "refs/heads/"))
          collect (let ((branch (subseq name 11)))
                    (update-ref repo (format nil "refs/remotes/~a/~a" remote branch) sha)
                    (cons branch sha))))

(defun fetch (repo &key url identity)
  "Fetch new objects from URL (default: remote origin) and advance the
   remote-tracking refs.  Returns the list of (branch . sha) updated."
  (let ((url (or url (remote-url repo) (error "cairn: no url and no remote origin"))))
    (multiple-value-bind (remote-refs head-target pack)
        (if (ssh-url-p url) (fetch-ssh repo url identity) (fetch-http repo url))
      (declare (ignore head-target))
      (if (null pack)
          (progn (format t "~&already up to date~%") '())
          (progn
            (format t "~&received packfile: ~d bytes~%" (length pack))
            (multiple-value-bind (name count) (index-pack repo pack)
              (format t "indexed ~a: ~d objects~%" name count))
            (setf (repo-packs repo) :unloaded)   ; new pack on disk: drop the stale cache
            (let ((updates (update-remote-refs repo remote-refs)))
              (format t "updated ~d remote-tracking ref~:p~%" (length updates))
              updates))))))

;;; ---- pull (fetch + fast-forward) --------------------------------------------

(defun ancestor-p (repo ancestor commit &optional (limit 10000))
  "Is ANCESTOR reachable from COMMIT by parent links?"
  (let ((seen (make-hash-table :test 'equal)) (queue (list commit)) (n 0))
    (loop while (and queue (< n limit)) do
      (let ((sha (pop queue)))
        (cond ((string= sha ancestor) (return-from ancestor-p t))
              ((gethash sha seen))
              (t (setf (gethash sha seen) t) (incf n)
                 (handler-case
                     (dolist (p (commit-parents (parse-commit (object-data repo sha))))
                       (push p queue))
                   (error () nil))))))
    nil))

;; pull lives in merge.lisp — it is fetch + merge, and merge loads after fetch.
