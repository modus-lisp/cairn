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
  :depends-on ("seal" "chipz" "salza2" "sb-posix")
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
     (:file "write")
     (:file "index")
     (:file "index-pack")
     (:file "checkout")
     (:file "commit")
     (:file "pktline")
     (:file "http")
     (:file "clone")))))
