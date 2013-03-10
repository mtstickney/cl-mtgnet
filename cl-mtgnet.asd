;;;; cl-rpc.asd

(asdf:defsystem #:cl-mtgnet
  :serial t
  :description "Describe cl-rpc here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:trivial-utf-8
               #:cl-json
               #:cl-netstring+
               #:usocket)
  :components ((:file "package")
               (:file "marshall")
               (:file "cl-mtgnet")))
