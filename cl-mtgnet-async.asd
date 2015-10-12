(asdf:defsystem #:cl-mtgnet-async
  :serial t
  :description "Asynchronous transport for the MTGNet client library."
  :author "Matthew Stickney <mtstickney@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-mtgnet
               #:cl-async)
  :components ((:file "transport-async")))
