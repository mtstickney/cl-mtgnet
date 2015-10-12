(asdf:defsystem #:cl-mtgnet-sync
  :serial t
  :description "Synchronous transport for the MTGNet client library."
  :author "Matthew Stickney <mtstickney@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-mtgnet
               #:usocket)
  :components ((:file "transport-sync")))
