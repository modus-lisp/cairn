;;;; merge.lisp — three-way merge (canonical git behaviour, pluggable resolver).
;;;;
;;;; git's default merge is three-way: find the merge base (the common ancestor
;;;; of the two commits), then for every path reconcile base/ours/theirs.  A
;;;; path only one side touched takes that side; a path both sides changed the
;;;; same way takes the shared result; a path both sides changed differently is
;;;; merged line-by-line (diff3) against the base, and any region both sides
;;;; edited is a conflict.
;;;;
;;;; Where merge stops being mechanical and starts being a judgement call — the
;;;; conflict — is deliberately a single seam: *MERGE-RESOLVER*.  The default
;;;; reproduces git (emit <<<<<<< / ======= / >>>>>>> markers and flag the
;;;; conflict); a smarter resolver (union, ours/theirs, or something that
;;;; actually understands the change) can be bound in its place without touching
;;;; the rest of the machinery.

(in-package #:cairn)

;;; ---- merge base(s) ----------------------------------------------------------
;;;
;;; When two branches criss-cross, they can share *several* best common
;;; ancestors — the merge base is not unique.  Picking one arbitrarily can
;;; silently drop a divergent resolution.  git's answer (the "recursive"
;;; strategy) is to merge the bases with each other first, producing one virtual
;;; ancestor tree to merge ours/theirs against.  Merging bases is itself a merge,
;;; so the whole thing recurses, and a base may be a *virtual* commit (a vnode).

(defstruct (vnode (:constructor make-vnode (tree parents id))) tree parents id)

(defun cnode-id (c) (if (vnode-p c) (vnode-id c) c))
(defun cnode-tree (repo c)
  (if (vnode-p c) (vnode-tree c) (commit-tree (parse-commit (object-data repo c)))))
(defun cnode-parents (repo c)
  (if (vnode-p c) (vnode-parents c)
      (handler-case (commit-parents (parse-commit (object-data repo c))) (error () nil))))

(defun cnode-ancestors (repo c)
  "id -> cnode for C and all of its ancestors."
  (let ((seen (make-hash-table :test 'equal)) (q (list c)))
    (loop while q do
      (let ((x (pop q)))
        (unless (nth-value 1 (gethash (cnode-id x) seen))
          (setf (gethash (cnode-id x) seen) x)
          (dolist (p (cnode-parents repo x)) (push p q)))))
    seen))

(defun all-merge-bases (repo a b)
  "The maximal common ancestors (best merge bases) of cnodes A and B — the
   common ancestors that are not themselves ancestors of another common one."
  (let ((anc-a (cnode-ancestors repo a)) (anc-b (cnode-ancestors repo b))
        (common (make-hash-table :test 'equal)) (redundant (make-hash-table :test 'equal))
        (result '()))
    (maphash (lambda (id c) (when (nth-value 1 (gethash id anc-b)) (setf (gethash id common) c))) anc-a)
    ;; a common node that is a *proper* ancestor of another common node is redundant
    (maphash (lambda (id c)
               (declare (ignore id))
               (dolist (p (cnode-parents repo c))
                 (maphash (lambda (aid ac) (declare (ignore ac))
                            (when (nth-value 1 (gethash aid common)) (setf (gethash aid redundant) t)))
                          (cnode-ancestors repo p))))
             common)
    (maphash (lambda (id c) (unless (nth-value 1 (gethash id redundant)) (push c result))) common)
    result))

(defun merge-base (repo a b)
  "One best merge base of commits A and B (a sha), or NIL."
  (let ((bs (all-merge-bases repo a b))) (and bs (cnode-id (first bs)))))

;;; ---- three-way content merge (diff3) ----------------------------------------

(defparameter *merge-ours-label* "HEAD")
(defparameter *merge-theirs-label* "theirs")

(defun conflict-markers (ours base theirs)
  "The default resolver: reproduce git's conflict markers, flag a conflict."
  (declare (ignore base))
  (values (append (list (format nil "<<<<<<< ~a" *merge-ours-label*))
                  ours (list "=======") theirs
                  (list (format nil ">>>>>>> ~a" *merge-theirs-label*)))
          t))

(defvar *merge-resolver* #'conflict-markers
  "Called on a region both sides edited: (ours-lines base-lines theirs-lines) ->
   (values RESULT-LINES CONFLICTED-P).  Bind to change conflict handling.")

(defun lcs-matches (a b)
  "List of (a-index . b-index) for the lines A and B have in common (LCS order)."
  (let ((ai 0) (bi 0) (out '()))
    (dolist (cell (lcs-script a b) (nreverse out))
      (ecase (car cell)
        (:eq (push (cons ai bi) out) (incf ai) (incf bi))
        (:del (incf ai)) (:add (incf bi))))))

(defun diff3-merge (o a b)
  "Three-way merge of line-vectors O (base), A (ours), B (theirs).  Returns
   (values MERGED-LINE-LIST CONFLICTED-P)."
  (let ((oa (make-hash-table)) (ob (make-hash-table)) (out '()) (conflicted nil))
    (loop for (oi . ai) in (lcs-matches o a) do (setf (gethash oi oa) ai))
    (loop for (oi . bi) in (lcs-matches o b) do (setf (gethash oi ob) bi))
    (let ((anchors (sort (loop for oi being the hash-keys of oa
                               when (nth-value 1 (gethash oi ob)) collect oi)
                         #'<))
          (po -1) (pa -1) (pb -1))
      (flet ((emit (lst) (setf out (nconc out lst))))
        (labels ((region (o2 a2 b2)
                   (let ((ro (coerce (subseq o (1+ po) o2) 'list))
                         (ra (coerce (subseq a (1+ pa) a2) 'list))
                         (rb (coerce (subseq b (1+ pb) b2) 'list)))
                     (cond ((equal ra ro) (emit rb))          ; only theirs changed
                           ((equal rb ro) (emit ra))          ; only ours changed
                           ((equal ra rb) (emit ra))          ; same change both sides
                           (t (multiple-value-bind (lines c) (funcall *merge-resolver* ra ro rb)
                                (emit lines) (when c (setf conflicted t))))))))
          (dolist (oi anchors)
            (let ((ai (gethash oi oa)) (bi (gethash oi ob)))
              (region oi ai bi)
              (emit (list (aref o oi)))                       ; the shared anchor line
              (setf po oi pa ai pb bi)))
          (region (length o) (length a) (length b)))))        ; trailing region
    (values out conflicted)))

;;; ---- three-way tree merge ---------------------------------------------------

(defun tree-path-map (repo tree-sha)
  "Path -> (mode . sha) for every blob under TREE-SHA (NIL tree -> empty)."
  (let ((m (make-hash-table :test 'equal)))
    (when tree-sha (dolist (e (flatten-tree repo tree-sha)) (setf (gethash (car e) m) (cdr e))))
    m))

(defun blob-entry-p (entry) (member (car entry) '("100644" "100755") :test #'string=))

(defun blob-lines (repo entry)
  (if entry (lines-of (object-data repo (cdr entry))) (make-array 0)))

(defun merge-blob-entries (repo o a b)
  "Content-merge file entries O/A/B (mode . sha).  Returns (values (mode . sha)
   CONFLICTED-P) — the new blob (with markers baked in if conflicted)."
  (multiple-value-bind (lines conflicted) (diff3-merge (blob-lines repo o) (blob-lines repo a) (blob-lines repo b))
    (let ((text (with-output-to-string (s) (dolist (l lines) (write-line l s)))))
      (values (cons (car a) (write-object repo :blob (string->bytes text))) conflicted))))

(defun merge-flat-trees (repo base-tree ours-tree theirs-tree)
  "Reconcile the three trees path by path.  Returns (values RESULT CONFLICTS):
   RESULT is a path->(mode . sha) table; CONFLICTS a list of (path o a b)."
  (let ((ob (tree-path-map repo base-tree)) (om (tree-path-map repo ours-tree))
        (tm (tree-path-map repo theirs-tree))
        (result (make-hash-table :test 'equal)) (conflicts '()) (paths (make-hash-table :test 'equal)))
    (dolist (h (list ob om tm)) (maphash (lambda (p v) (declare (ignore v)) (setf (gethash p paths) t)) h))
    (maphash
     (lambda (p _)
       (declare (ignore _))
       (let ((o (gethash p ob)) (a (gethash p om)) (b (gethash p tm)))
         (cond
           ((equal a b) (when a (setf (gethash p result) a)))         ; identical (incl. both deleted)
           ((equal o a) (when b (setf (gethash p result) b)))         ; ours untouched -> theirs
           ((equal o b) (when a (setf (gethash p result) a)))         ; theirs untouched -> ours
           ((and a b (blob-entry-p a) (blob-entry-p b))               ; both edited a file
            (multiple-value-bind (merged conflicted) (merge-blob-entries repo o a b)
              (setf (gethash p result) merged)
              (when conflicted (push (list p o a b) conflicts))))
           (t                                                         ; modify/delete, add/add, mode/type clash
            (push (list p o a b) conflicts)
            (when (or a b) (setf (gethash p result) (or a b)))))))
     paths)
    (values result conflicts)))

(defun write-tree-from-map (repo result)
  "Write tree objects for a path->(mode . sha) map; return the root tree SHA."
  (let ((entries '()))
    (maphash (lambda (path ms)
               (push (make-index-entry :path path :sha (cdr ms)
                                       :mode (tree-mode->index-mode (car ms)))
                     entries))
             result)
    (build-tree repo entries "")))

;;; ---- recursive base folding -------------------------------------------------

(defvar *vnode-id* 0)

(defun merge-two (repo a b)
  "Recursively merge cnodes A and B into a virtual commit (vnode) whose tree is
   their three-way merge (conflicts baked into the content, as git does for a
   virtual ancestor)."
  (let* ((bases (all-merge-bases repo a b))
         (base-tree (virtual-base-tree repo bases))
         (result (merge-flat-trees repo base-tree (cnode-tree repo a) (cnode-tree repo b))))
    (make-vnode (write-tree-from-map repo result) (list a b) (format nil "v~d" (incf *vnode-id*)))))

(defun virtual-base-tree (repo bases)
  "Fold the merge BASES (cnodes) into a single virtual-ancestor TREE sha: no
   bases -> the empty tree; one -> its tree; several -> merge them together."
  (cond ((null bases) (write-tree-from-map repo (make-hash-table :test 'equal)))
        ((null (cdr bases)) (cnode-tree repo (first bases)))
        (t (cnode-tree repo (reduce (lambda (acc b) (merge-two repo acc b))
                                    (rest bases) :initial-value (first bases))))))

;;; ---- driver -----------------------------------------------------------------

(defun current-branch-name (repo)
  (multiple-value-bind (kind ref) (head-ref repo)
    (if (eq kind :symbolic) (subseq ref (1+ (or (position #\/ ref :from-end t) -1))) "HEAD")))

(defun write-worktree-file (repo path mode sha)
  (let ((abs (worktree-path repo path)))
    (multiple-value-bind (type content) (read-object repo sha)
      (declare (ignore type))
      (if (string= mode "120000")
          (progn (uiop:delete-file-if-exists abs) (sb-posix:symlink (ascii content) (native abs)))
          (progn (write-bytes abs content)
                 (when (string= mode "100755") (sb-posix:chmod (native abs) #o755)))))))

(defun finish-clean-merge (repo tree ours theirs label message author committer)
  (let* ((msg (or message (format nil "Merge ~a into ~a" label (current-branch-name repo))))
         (now (unix-now))
         (text (with-output-to-string (s)
                 (format s "tree ~a~%parent ~a~%parent ~a~%" tree ours theirs)
                 (format s "author ~a ~d +0000~%committer ~a ~d +0000~%~%~a~%" author now committer now msg)))
         (sha (write-object repo :commit (sb-ext:string-to-octets text :external-format :utf-8))))
    (multiple-value-bind (kind ref) (head-ref repo)
      (when (eq kind :symbolic) (update-ref repo ref sha)))
    (checkout repo sha)
    (format t "~&Merge made by the 'three-way' strategy: ~a~%" (short sha))
    sha))

(defun finish-conflicted-merge (repo result conflicts theirs label)
  (let ((entries '()) (cpaths (mapcar #'first conflicts)))
    ;; working tree from the merged result (conflicted files carry their markers)
    (maphash (lambda (path ms)
               (write-worktree-file repo path (car ms) (cdr ms))
               (unless (member path cpaths :test #'string=)
                 (push (stat-index-entry (worktree-path repo path) path (cdr ms) (car ms))
                       entries)))
             result)
    ;; conflicted paths as index stages 1 (base) / 2 (ours) / 3 (theirs)
    (dolist (c conflicts)
      (destructuring-bind (path o a b) c
        (flet ((stage (entry n)
                 (when (and entry (blob-entry-p entry))
                   (push (make-index-entry :path path :stage n :sha (cdr entry)
                                           :mode (tree-mode->index-mode (car entry))
                                           :ctime 0 :mtime 0 :dev 0 :ino 0 :uid 0 :gid 0 :size 0)
                         entries))))
          (stage o 1) (stage a 2) (stage b 3))))
    (write-index repo entries)
    (write-text-file (merge-pathnames "MERGE_HEAD" (repo-git-dir repo)) (format nil "~a~%" theirs))
    (write-text-file (merge-pathnames "MERGE_MSG" (repo-git-dir repo))
                     (format nil "Merge ~a~%~%# Conflicts:~%~{#\	~a~%~}" label (sort (copy-list cpaths) #'string<)))
    (format t "~&Auto-merging: ~d conflict~:p~%~{CONFLICT (content): Merge conflict in ~a~%~}~
              Automatic merge failed; fix conflicts and then commit the result.~%"
            (length conflicts) (sort (copy-list cpaths) #'string<))
    (values :conflicts (sort (copy-list cpaths) #'string<))))

(defun merge (repo commitish &key message (author "cairn <cairn@localhost>") (committer author)
                                  (theirs-label commitish))
  "Merge COMMITISH into the current branch (canonical three-way, recursive over
   multiple merge bases).  Fast-forwards when possible; on a clean merge writes a
   two-parent merge commit and checks it out; on conflicts writes markered files
   + index stages + MERGE_HEAD and returns (values :conflicts PATHS).  Conflict
   handling is *merge-resolver*."
  (with-oid (repo)
  (let* ((theirs (rev-parse repo commitish))
         (ours (head-commit repo))
         (*merge-theirs-label* theirs-label))
    (cond
      ((string= ours theirs) (format t "~&Already up to date.~%") :up-to-date)
      ((ancestor-p repo theirs ours) (format t "~&Already up to date.~%") :up-to-date)
      ((ancestor-p repo ours theirs)
       (multiple-value-bind (kind ref) (head-ref repo)
         (when (eq kind :symbolic) (update-ref repo ref theirs)))
       (checkout repo theirs)
       (format t "~&Fast-forward to ~a~%" (short theirs))
       :fast-forward)
      (t
       (let* ((bases (all-merge-bases repo ours theirs))
              (base-tree (virtual-base-tree repo bases))
              (ours-tree (commit-tree (parse-commit (object-data repo ours))))
              (theirs-tree (commit-tree (parse-commit (object-data repo theirs)))))
         (when (cdr bases)
           (format t "~&Merging ~d common ancestors (recursive)~%" (length bases)))
         (multiple-value-bind (result conflicts) (merge-flat-trees repo base-tree ours-tree theirs-tree)
           (if conflicts
               (finish-conflicted-merge repo result conflicts theirs theirs-label)
               (finish-clean-merge repo (write-tree-from-map repo result)
                                   ours theirs theirs-label message author committer)))))))))

;;; ---- pull (fetch + merge) ---------------------------------------------------

(defun pull (repo &key url identity message
                       (author "cairn <cairn@localhost>") (committer author))
  "git pull = fetch + merge: fetch the upstream branch, then merge it into the
   current branch — fast-forward when possible, otherwise a three-way merge
   (a clean merge makes a merge commit; a conflict is left in the tree with
   markers + an unmerged index, exactly as git leaves it)."
  (fetch repo :url url :identity identity)
  (multiple-value-bind (kind ref) (head-ref repo)
    (unless (eq kind :symbolic) (error "cairn: detached HEAD, cannot pull"))
    (let* ((branch (subseq ref (length "refs/heads/")))   ; full branch path, nested ok
           (remote-sha (read-ref-file repo (format nil "refs/remotes/origin/~a" branch))))
      (cond
        ((null remote-sha) (format t "~&no upstream for ~a~%" branch) nil)
        ((null (ignore-errors (head-commit repo)))            ; unborn branch: adopt upstream
         (update-ref repo ref remote-sha) (checkout repo remote-sha) remote-sha)
        (t (merge repo remote-sha
                  :theirs-label (format nil "origin/~a" branch)
                  :message (or message
                               (format nil "Merge branch '~a' of ~a into ~a"
                                       branch (or url (remote-url repo) "origin") branch))
                  :author author :committer committer))))))
