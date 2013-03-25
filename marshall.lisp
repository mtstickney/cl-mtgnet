(in-package #:cl-mtgnet)

(defun encode-field (field value &optional (encoder #'json:encode-json))
  "Encode VALUE as an object field named FIELD, using ENCODER to
encode VALUE."
  (json:as-object-member (field)
    (funcall encoder value)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun serial-slot-p (s)
    (or (symbolp s)
        (and (listp s)
             (destructuring-bind (name &key (marshall t) &allow-other-keys)
                 s
               (declare (ignore name))
               marshall))))

  (defun optional-slot-p (s)
    (and (listp s)
         (destructuring-bind (name &key optional &allow-other-keys)
             s
           (declare (ignore name))
           optional)))

  (defun slot-symbol (s)
    (etypecase s
      (symbol s)
      (list (destructuring-bind (name &rest r) s
              (declare (ignore r))
              name))))

  (defun cat-symbol (symbol &rest rest)
  (intern (format nil "~{~A~}" (mapcar #'symbol-name
                                       (cons symbol rest)))))

  (defun slot-optional-p (symbol optional-slots type)
    (check-type symbol symbol)
    (check-type optional-slots list)
    (check-type type keyword)
    (let ((cell (assoc symbol optional-slots)))
      (and cell
           (ecase type
             (:write
              (or (eq (cdr cell) :write)
                  (eq (cdr cell) :read-write)))
             (:read
              (or (eq (cdr cell) :read)
                  (eq (cdr cell) :read-write)))
             (:read-write (eq (cdr cell) :read-write)))))))

;; TODO: See if it's possible to generate docstrings for these things
(defmacro define-json-obj (name &body body)
  (let* ((slots (if (stringp (first body))
                    (first (cdr body))
                    (first body)))
         (serialized-slots (mapcar #'slot-symbol
                                   (remove-if-not #'serial-slot-p slots)))
         (optional-slots (remove-if #'null (mapcar (lambda (s)
                                                     (let ((optional (optional-slot-p s)))
                                                       (if optional
                                                           (cons (slot-symbol s) optional)
                                                           nil)))
                                                   slots)))
         (make-func (cat-symbol '#:make- name))
         (build-func (cat-symbol '#:build- name))
         (unmarshall-func (cat-symbol '#:unmarshall- name))
         (marshall-func (cat-symbol '#:marshall- name)))
    `(progn
       (defstruct ,name
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
                           ,@(unless (slot-optional-p s optional-slots :read)
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
           ,@(flet ((accessor (field)
                              (intern (format nil "~A-~A" name field))))
                   (loop for s in serialized-slots
                      collect (let ((encode-form `(encode-field ',s (,(accessor s) obj))))
                                (if (slot-optional-p s optional-slots :write)
                                    `(when (,(accessor s) obj)
                                       ,encode-form)
                                    encode-form)))))))))

(define-json-obj rpc-call
  "A single method invocation."
  (service :initial "" :type string :read-only t)
  (method :initial "" :type string :read-only t)
  (args :initial '() :type arglist :read-only t)
  (id :initial nil :read-only t))

