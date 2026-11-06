;;;; http.lisp — git's HTTP needs, on seal/http.
;;;;
;;;; This file used to be a small HTTP/1.1 client of its own — one of four in the
;;;; workspace, because there was nowhere shared to put one.  seal/http is that
;;;; place now, so all that is left here is the shape git's smart transport
;;;; wants: (VALUES CODE HEADERS BODY) rather than a response object.
;;;;
;;;; Redirects are OFF deliberately.  The old code could not follow them at all,
;;;; and following them silently would change what a push does: a 301 on a POST
;;;; degrades to GET under the usual rules, which against git-receive-pack means
;;;; quietly not pushing.  If git redirects ever matter here they should be
;;;; handled explicitly, per-verb, rather than inherited.

(in-package #:cairn)

(defun http-request (method url &key headers body)
  "Perform METHOD against URL over seal/http.  Returns (values STATUS-CODE
   HEADERS-ALIST BODY-BYTES)."
  (let ((r (seal.http:request method url :headers headers :body body
                                         :max-redirects 0)))
    (values (seal.http:response-status r)
            (seal.http:response-headers r)
            (seal.http:response-body r))))

(defun http-open-body (method url &key headers body)
  "Like HTTP-REQUEST, but the body is not read: returns (values STATUS-CODE HEADERS-ALIST
   READ CLOSE), where (READ BUF START END) pulls the next octets of the body into BUF and
   returns how many (0 at the end), and (CLOSE) must be called when done."
  (multiple-value-bind (read code hdrs close)
      (seal.http:open-body-reader method url :headers headers :body body)
    (values code hdrs read close)))
