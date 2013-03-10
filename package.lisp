;;;; package.lisp

(defpackage #:cl-mtgnet
  (:use #:cl)
  (:export #:define-rpc-method
           #:with-batch-calls
           #:wait))
