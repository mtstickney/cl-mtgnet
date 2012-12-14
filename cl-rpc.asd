;;;; cl-rpc.asd

(asdf:defsystem #:cl-rpc
  :serial t
  :description "Describe cl-rpc here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:trivial-utf-8
               #:cl-json)
  :components ((:file "package")
               (:file "cl-rpc")))
