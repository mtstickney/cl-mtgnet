;;;; cl-mtgnet.asd

(asdf:defsystem #:cl-mtgnet
  :serial t
  :description "Client library for the MTGNet RPC protocol."
  :author "Matthew Stickney <mtstickney@gmail.com>"
  :license "MIT"
  :depends-on (#:trivial-utf-8
               #:cl-json
               #:cl-netstring+
               #:usocket)
  :components ((:file "package")
               (:file "marshall")
               (:file "transport")
               (:file "cl-mtgnet")))
