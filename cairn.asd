;;;; cairn.asd — a git implementation in pure Common Lisp.

(asdf:defsystem :cairn
  :description "A clean-room git in pure Common Lisp: the object model, loose and
packed object stores, refs, the index, add/commit, checkout, and clone over
smart HTTP.  SHA-1 comes from the sibling `seal` (the classical-crypto home),
DEFLATE from `chipz`/`salza2` (pure-CL inflate/deflate), TLS from `seal` — no
libgit2, no libcurl, no shelling out to git, no FFI."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("seal" "conch" "chipz" "salza2" "sb-posix")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "util")
     (:file "objects")
     (:file "fs-backend")
     (:file "refs")
     (:file "pack")
     (:file "repository")
     (:file "plumbing")
     (:file "write")
     (:file "index")
     (:file "index-pack")
     (:file "pack-write")
     (:file "checkout")
     (:file "commit")
     (:file "status")
     (:file "diff")
     (:file "pktline")
     (:file "http")
     (:file "clone")
     (:file "ssh")
     (:file "fetch")
     (:file "merge")
     (:file "serve")))))

;;; Opt-in bridge: keep a cairn repository inside a cabinet filesystem (which
;;; lives in a single pagetree file).  Separate system so cairn's core carries no
;;; dependency on cabinet/pagetree unless you ask for it.
(asdf:defsystem :cairn/cabinet
  :description "A cairn storage backend that keeps the git store in a cabinet
filesystem (backed by a pagetree file) instead of the host filesystem."
  :depends-on ("cairn" "cabinet")
  :serial t
  :components ((:module "src" :serial t :components ((:file "backend-cabinet")))))

;; Opt-in: keep the git object/ref store DIRECTLY in pagetree KV (no filesystem
;; layer), which unlocks operation-level atomic commit/push (one store txn).
(asdf:defsystem :cairn/pagetree
  :description "A cairn storage backend that keeps the git object/ref store
directly in a pagetree key-value store, with atomic commit/push."
  :depends-on ("cairn" "pagetree")
  :serial t
  :components ((:module "src" :serial t :components ((:file "backend-pagetree")))))
