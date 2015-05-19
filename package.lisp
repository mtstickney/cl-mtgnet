;;;; package.lisp

(defpackage #:mtgnet-sys
  (:use #:cl)
  ;; Transport API
  (:export #:transport
           #:synchronous-transport
           #:asynchronout-transport
           #:transport-connect
           #:transport-disconnect
           #:transport-read
           #:transport-read-into!
           #:transport-write)
  ;; Builtin transports
  (:export #:string-output-transport
           #:string-input-transport
           ;; Synchronous network transports
           #:synchronous-tcp-transport
           #:synchronous-tcp-byte-transport
           #:tcp-address
           #:tcp-port
           #:socket
           #:tcp-element-type
           ;; Asynchronous network transports
           #:asynchronous-tcp-transport
           #:socket-stream
           #:read-cb
           #:write-cb
           #:error-cb
           #:unregister-op!)
  ;; Framer API
  (:export #:data-framer
           #:frame-data
           #:frame-data*
           #:unframe-data
           #:write-frame
           #:read-frame)
  ;; Builtin framers
  (:export #:netstring-framer
           #:netstring-state)
  ;; Basic API
  (:export #:rpc-connection
           #:connect
           #:disconnect
           #:invoke-rpc-method
           #:with-batch-calls
           #:wait
           #:duplicate-result-id
           #:event-loop-exited
           #:remote-warning
           #:remote-warning-msg
           #:remote-warning-code
           #:remote-error-type
           #:remote-error-msg
           #:remote-error-code
           #:invalid-json-obj
           #:*default-encoder*)
  ;; Extension points
  (:export #:send-response
           #:read-response
           #:send-request
           #:read-request
           #:add-result)
  ;;; Things that extensions might use.
  ;; Standard objects.
  (:export #:rpc-call-p
           #:rpc-call-service
           #:rpc-call-id
           #:rpc-call-method
           #:rpc-call-args
           #:build-rpc-call
           #:make-rpc-call
           #:marshall-rpc-call
           #:unmarshall-rpc-call

           #:rpc-error-p
           #:rpc-error-message
           #:rpc-error-code
           #:rpc-error-data
           #:build-rpc-error
           #:make-rpc-error
           #:marshall-rpc-error
           #:unmarshall-rpc-error

           #:rpc-result
           #:rpc-result-p
           #:rpc-result-data
           #:rpc-result-error
           #:rpc-result-warnings
           #:rpc-result-id
           #:build-rpc-result
           #:make-rpc-result
           #:marshall-rpc-result
           #:unmarshall-rpc-result

           #:rpc-request
           #:rpc-request-p
           #:build-rpc-request
           #:marshall-rpc-request
           #:unmarshall-rpc-request

           #:rpc-response
           #:rpc-response-p
           #:build-rpc-response
           #:marshall-rpc-response
           #:unmarshall-rpc-response

           #:rpc-warning-list
           #:rpc-warning-list-p
           #:build-rpc-warning-list
           #:marshall-rpc-warning-list
           #:unmarshall-rpc-warning-list

           #:send-frame
           #:send-frame*
           #:receive-frame
           #:process-next-response)
  ;; Utility functions
  (:export #:make-result-promise
           #:rpc-call-promise
           #:make-call-obj))

(defpackage #:cl-mtgnet
  (:use #:cl)
  (:nicknames #:mtgnet)
  (:import-from #:mtgnet-sys
                #:rpc-connection
                #:connect
                #:disconnect
                #:invoke-rpc-method
                #:with-batch-calls
                #:wait
                #:duplicate-result-id
                #:event-loop-exited
                #:remote-warning
                #:remote-warning-msg
                #:remote-warning-code
                #:remote-error-type
                #:remote-error-msg
                #:remote-error-code
                #:invalid-json-obj
                #:*default-encoder*)
  (:export #:rpc-connection
           #:connect
           #:disconnect
           #:invoke-rpc-method
           #:with-batch-calls
           #:wait
           #:duplicate-result-id
           #:event-loop-exited
           #:remote-warning
           #:remote-warning-msg
           #:remote-warning-code
           #:remote-error-type
           #:remote-error-msg
           #:remote-error-code
           #:invalid-json-obj
           #:*default-encoder))
