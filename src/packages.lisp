;;;; packages.lisp — cairn

(defpackage #:cairn
  (:use #:cl)
  (:documentation
   "cairn — a git implementation in pure Common Lisp.  A cairn is a trail marker
    built one stone at a time; a git history is the same — each commit a stone,
    the chain of them marking where the work has been.  Clean-room: the object
    model, the loose/packed object stores, refs, the index, and (later) the pack
    transfer protocol, all from scratch.  No libgit2, no shelling out to git, no
    FFI.  Its own SHA-1 (git's content-address hash) and its own DEFLATE.")
  (:export
   ;; hashing / compression primitives
   #:sha1 #:sha1-hex #:zlib-decompress #:zlib-compress #:inflate #:deflate
   ;; repository
   #:open-repository #:repository #:repo-path #:with-repository
   ;; objects
   #:read-object #:object-type #:object-data #:hash-object
   #:parse-commit #:parse-tree
   #:commit-tree #:commit-parents #:commit-message #:commit-author #:commit-committer
   #:commit-summary #:tree-entries #:tree-entry-mode #:tree-entry-name #:tree-entry-sha
   ;; refs
   #:resolve-ref #:head-commit #:list-refs #:ref-target
   ;; plumbing / porcelain
   #:cat-file #:cat-file-string #:log-commits #:ls-tree #:rev-parse
   ;; transport (smart HTTP over seal)
   #:clone #:discover-refs #:fetch-pack #:index-pack))
