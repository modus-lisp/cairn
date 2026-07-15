;;;; cairn.asd — a git implementation in pure Common Lisp.

(asdf:defsystem :cairn
  :description "A clean-room git in pure Common Lisp: the object model, loose and
packed object stores, refs, the index, and pack transfer.  Its own SHA-1 and
DEFLATE.  No libgit2, no shelling out to git, no FFI."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ()
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "sha1")
     (:file "inflate")
     (:file "objects")
     (:file "refs")
     (:file "pack")
     (:file "repository")
     (:file "plumbing")))))
