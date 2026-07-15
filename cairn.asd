;;;; cairn.asd — a git implementation in pure Common Lisp.

(asdf:defsystem :cairn
  :description "A clean-room git in pure Common Lisp: the object model, loose and
packed object stores, refs, the index, and pack transfer.  SHA-1 comes from the
sibling `seal` (the classical-crypto home), DEFLATE from `chipz` (pure-CL
inflate) — no libgit2, no shelling out to git, no FFI."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("seal" "chipz")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "util")
     (:file "objects")
     (:file "refs")
     (:file "pack")
     (:file "repository")
     (:file "plumbing")
     (:file "pktline")
     (:file "http")
     (:file "index-pack")
     (:file "clone")))))
