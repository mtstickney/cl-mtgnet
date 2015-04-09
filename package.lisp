;;;; package.lisp

(defpackage #:cl-mtgnet
  (:use #:cl)
  (:nicknames #:mtgnet)
  (:export #:define-rpc-method
           #:with-batch-calls
           #:wait))
