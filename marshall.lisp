(in-package #:cl-mtgnet)

(defun encode-field (field value &optional (encoder #'json:encode-json))
  "Encode VALUE as an object field named FIELD, using ENCODER to
encode VALUE."
  (json:as-object-member (field)
    (funcall encoder value)))

(defun serial-slot-p (s)
  "Return T if S is a definition for a serialized slot, or NIL otherwise."
  (or (symbolp s)
      (and (listp s)
           (getf s :marshall t))))

(defun optional-slot-p (s)
  "Return the optional type keyword for the slot-definition
S (:read, :write, or :both), or NIL if S is not an optional slot."
  (and (listp s)
       (getf s :optional nil)))

(defun slot-optional-type (slot type)
  (check-type slot (or list symbol))
  (check-type type keyword)
  (and (listp slot)
       (let ((slot-type (optional-slot-p slot)))
         (ecase type
           (:write
            (or (eq slot-type :write)
                (eq slot-type :both)))
           (:read
            (or (eq slot-type :read)
                (eq slot-type :both)))
           (:both (eq slot-type :both))))))

(defun slot-symbol (slot)
  (if (symbolp slot)
      slot
      (car slot)))

(defun cat-symbol (symbol &rest rest)
  (intern (format nil "~{~A~}" (mapcar #'symbol-name
                                       (cons symbol rest)))))

;; TODO: See if it's possible to include docstrings for these things
(defmacro define-json-obj (name &body body)
  (let* ((slots (if (stringp (first body))
                    (first (cdr body))
                    (first body)))
         (serialized-slots (remove-if-not #'serial-slot-p slots))
         (struct-opts (if (stringp (first body))
                          (nthcdr 2  body)
                          (nthcdr 1 body)))
         (default-initargs (let ((cell (assoc :default-initargs struct-opts)))
                             (if cell (cdr cell) nil)))
         (make-func (cat-symbol '#:make- name))
         (ctor (cat-symbol '#:* make-func))
         (build-func (cat-symbol '#:build- name))
         (unmarshall-func (cat-symbol '#:unmarshall- name))
         (marshall-func (cat-symbol '#:marshall- name)))
    `(progn
       (defstruct (,name (:constructor ,ctor))
         ;; Optional docstring
         ,@(if (stringp (first body))
               (list (first body))
               nil)
         ;; Slot list
         ,@(loop for slot in slots
              collect
                (etypecase slot
                  (symbol slot)
                  (list
                   (destructuring-bind (name &rest keyword-pairs &key initial
                                             &allow-other-keys)
                       slot
                     `(,name ,initial :allow-other-keys t ,@keyword-pairs))))))
       ;; Produce a structure from a decoded json object
       (defun ,build-func (json-obj)
         (let ((arglist '()))
           ,@(loop for s in serialized-slots
                collect `(let ((cell (assoc (json-key ',s) json-obj)))
                           ,@(unless (slot-optional-type s :read)
                                     `((unless cell
                                         (error 'invalid-json-obj :type ',name :json json-obj))))
                           (when cell
                             (setf (getf arglist ,(intern (symbol-name s) 'keyword))
                                   (cdr cell)))))
           (apply #',make-func arglist)))
       (defun ,unmarshall-func (json-string)
         (let ((json-obj (json:decode-json-from-string json-string)))
           (,build-func json-obj)))
       (defun ,marshall-func (obj)
         (json:with-object ()
           ,@(flet ((accessor (field) (cat-symbol name '#:- field)))
                   (loop for s in serialized-slots
                      collect (let ((encode-form `(encode-field ',s (,(accessor s) obj))))
                                (if (slot-optional-type s :write)
                                    `(when (,(accessor s) obj)
                                       ,encode-form)
                                    encode-form))))))
       (defun ,make-func (&rest initargs)
         ;; Explicitly defined options take precedence, otherwise use :default-initargs
         (let ((initargs (concatenate 'list initargs ,(cdr default-initargs))))
               (apply #',ctor initargs))))))

(define-json-obj rpc-call
  "A single method invocation."
  ((service :initial "" :type string :read-only t)
   (method :initial "" :type string :read-only t)
   (args :initial '() :type arglist :read-only t)
   (id :initial nil :read-only t)))

(define-json-obj rpc-result
    "A single result from a method invocation."
  ((data :initial '() :type cons :read-only t :marshall nil)
   (warnings :initial '() :type list :read-only t)
   (encoder :intiial #'json:encode-json :type function :marshall nil)
   (id :optional :both))
  (:default-initargs
      :data 
      )
    )

(defstruct rpc-result
  "A single result from a method invocation."
  ;; either (:DATA . data) or (:ERROR . error-obj)
  (data '() :type cons)
  (warnings '() :type list)
  (encoder #'json:encode-json :type function)
  (id))

(defmacro encode-fields ((obj) &rest fields)
  "JSON-encode the FIELDS slots of OBJ. Expects to be run inside a
JSON:WITH-OBJECT block."
  (let ((obj-var (gensym)))
    `(let ((,obj-var ,obj))
       (progn
         ,@(loop for f in fields collect
                (if (listp f)
                    `(encode-field (quote ,(first f))
                                   (slot-value ,obj-var (quote ,(first f)))
                                   ,(second f))
                    `(encode-field (quote ,f)
                                   (slot-value ,obj-var (quote ,f)))))))))

(defun json-key (symb)
  "Return the symbol produced by JSON encoding and decoding SYMB."
  (funcall json::*identifier-name-to-key*
           (json:decode-json-from-string
            (funcall json::*json-identifier-name-to-lisp*
                     (json:encode-json-to-string symb)))))

(defmacro has-fields-p (alist &rest fields)
  "Search for all the FIELDS in the JSON-decoded object
ALIST. Expanded code returns T if all fields were found, NIL
otherwise."
  `(and
    ,@(loop for f in fields
         collect `(assoc (json-key (quote ,f))
                         ,alist))))

(defun get-field (key alist &rest args)
  "Return either the value of the field named KEY from the decoded
json object ALIST, or NIL. Additional arguments will be passed to
ASSOC."
  (let* ((key (json-key key))
         (cell (apply #'assoc (cons key (cons alist args)))))
    (and cell (cdr cell))))

;;; Error conditions
(define-condition invalid-json-obj ()
  ((type :initarg :type)
   (json :initarg :json)))

(define-condition invalid-call-object ()
  ((obj :initarg :obj)))

(define-condition invalid-result-obj ()
  ((obj :initarg :obj)))

;;; Call object, one per method call
;; TODO: Find a more stringent definition for arglist ('(list string)?)
(deftype arglist () `list)

(defstruct rpc-call
  "A single method invocation."
  (service "" :type string :read-only t)
  (method "" :type string :read-only t)
  (args '() :type arglist :read-only t)
  (id nil :read-only t))

(defun valid-call-p (obj)
  "Returns T if the object is a valid CALL object."
  (rpc-call-p obj))

(defun marshall-call (obj)
  "Marshall the call object OBJ to JSON."
  (check-type obj rpc-call)
  (json:with-object ()
    (encode-fields (obj)
                   service
                   method
                   args
                   id)))

(defun marshall-call (obj)
  (check-type obj rpc-call)
  (optima:match obj
    ((structure rpc-call- service method args id)
     (loop for f in '(service method args id)
        do (encode-field f (symbol-value f))))
    (_ (error "Unexpected RPC-CALL structure"))))

(defun build-call (json-obj)
  "Build an RPC-CALL object from the JSON-OBJ data."
  (when (not (has-fields-p json-obj service method args))
    (error 'invalid-json-obj :type 'rpc-call :json json-obj))
  (make-rpc-call :service (get-field 'service json-obj)
                 :method (get-field 'method json-obj)
                 :args (get-field 'args json-obj)
                 :id (get-field 'id json-obj)))

(defun build-call (json-obj)
  (optima:match json-obj
    ((alist ((json-key 'service) . service)
            ((json-key 'method) . method)
            ((json-key 'args) . args))
     (make-rpc-call :service service
                   :method method
                   :args args
                   :id (optima:match json-obj ((optima::assoc (json-key 'id) id) id))
                   :id (get-field 'id json-obj)))
                   ;; (optima::if-match
                   ;;     (optima::assoc (json-key 'id) id) json-obj
                   ;;   id
                   ;;   nil)))
    (_ (error 'invalid-json-obj :type 'rpc-call :json json-obj))))

(defun unmarshall-call (json-string)
  "Unmarshall a CALL object from the decoded JSON-STRING."
  (let ((json-obj (json:decode-json-from-string json-string)))
    (build-call json-obj)))

;;; Result object, one per (non-notification) method call
(defstruct rpc-result
  "A single result from a method invocation."
  ;; either (:DATA . data) or (:ERROR . error-obj)
  (data '() :type cons)
  (warnings '() :type list)
  (encoder #'json:encode-json :type function)
  (id))

(defun marshall-result (obj)
  (optima:match obj
    ((structure rpc-result- data warnings id)
     (loop for f in '(data warnings id)
        do (encode-field f (symbol-value f)))
     (let (key val)
       (optima:match data
         ((cons :data data)
          (setf key 'data
                val data))
         ((cons :error error)
          (setf key 'error
                val error))
         (_ (error "RPC-RESULT-DATA ~S is neither (:DATA . data) nor (:ERROR . error)")))
       (encode-field key val)))
    (_ (error "Unexpected RPC-RESULT structure"))))

(defun marshall-result (obj)
  "Marshall the RESULT object OBJ to JSON."
  (let ((result-data (rpc-result-data obj)))
    (when (or (not result-data)
              (not (consp result-data))
              (not (member (car result-data) '(:data :error)))
              (and (eql (car result-data) :error)
                   (null (cdr result-data))))
      (error 'invalid-result-obj :obj obj))
    (json:with-object ()
      (encode-fields (obj)
                     warnings
                     id)
      (if (eql (car result-data) :error)
          (encode-field 'error (cdr result-data))
          (encode-field 'data
                        (cdr result-data)
                        (rpc-result-encoder obj))))))

(defun build-result (json-obj)
  (optima:match json-obj
    ((alist ((json-key 'warnings) . warnings)))
    )
  )
(defun build-result (json-obj)
  "Build an RPC-RESULT object from the JSON-OBJ data."
  (let* ((result-error (get-field 'error json-obj))
         (result-data (if result-error
                          (cons :error result-error)
                          (cons :data (get-field 'data json-obj)))))
    (when (not (has-fields-p json-obj id))
      (error 'invalid-json-obj :obj json-obj))
    (when (and (not (has-fields-p json-obj data))
               (not (has-fields-p json-obj error)))
      (warn "Json obj ~S has neither :DATA nor :ERROR field, assuming (:DATA . NIL)"
            json-obj))
    (make-rpc-result :id (get-field 'id json-obj)
                     :warnings (get-field 'warnings json-obj)
                     :data result-data)))

(defun unmarshall-result (json-string)
  "Unmarshall a RESULT object from the decoded JSON-STRING."
  (let* ((json-obj (json:decode-json-from-string json-string)))
    (build-result json-obj)))


;;; Request, a list of CALLs
(defun rpc-request-p (obj)
  "Return T if OBJ is a REQUEST object, NIL otherwise."
  (and (listp obj)
       (every #'rpc-call-p obj)))

(deftype rpc-request () '(satisfies rpc-request-p))

(defun marshall-request (obj)
  (check-type obj rpc-request)
  (json:with-array ()
    (loop for call in obj do
         (json:as-array-member ()
           (marshall-call call)))))

(defun unmarhsall-request (json-string)
  "Unmarshall a REQUEST object from the decoded JSON object JSON-OBJ."
  (let ((json-obj (json:decode-json-from-string json-string)))
    (unless (listp json-obj)
      (error 'invalid-json-obj :type 'rpc-request :json json-obj))
    (mapcar #'build-call json-obj)))


;;; Response, a list of RESULTs
(defun rpc-response-p (obj)
  "Return T if OBJ is a RESPONSE object, NIL otherwise."
  (and (listp obj)
       (every #'rpc-result-p obj)))

(deftype rpc-response () '(and list (satisfies response-p)))

;; TODO: as convenient as this is, it seems like marshall-* should
;; return a string, rather than writing to json:*json-output*
(defun marshall-response (obj)
  "Marshall the RPC-RESPONSE object OBJ to JSON."
  (check-type obj rpc-response)
  (json:with-array ()
    (loop for result in obj
       do (json:as-array-member ()
            (marshall-result result)))))

(defun unmarshall-response (json-string)
  "Unmarshall a RESPONSE object from the decoded JSON object JSON-STRING."
  (let ((json-obj (json:decode-json-from-string json-string)))
    (unless (listp json-obj)
      (error 'invalid-json-obj :type 'rpc-response :json json-obj))
    (mapcar #'build-result json-obj)))
