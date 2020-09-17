;;;; metalock.lisp

(in-package #:metalock)

(defclass metalock (c2mop:standard-class)
  ())

(defclass special-slot (c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:validate-superclass ((class metalock) (metaclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class special-slot) (metaclass standard-class))
  t)

(defmethod c2mop:compute-effective-slot-definition ((class special-slot) name dslots)
  (declare (ignore name dslots))
  (let ((slot (call-next-method)))
    slot))

(defun make-special-slot (keys)
  (apply #'make-instance 'special-slot keys))

(defmethod c2mop:slot-value-using-class :around ((class metalock) object slotd)
  (if (eq (type-of slotd) 'special-slot)
      (call-next-method)
      (let* ((name (c2mop:slot-definition-name slotd))
             (lock (cdr (assoc name (slot-value object 'slot-locks)))))
        (read-with-rw-lock (lock)
          (call-next-method)))))

(defmethod (setf c2mop:slot-value-using-class) :around (new-value (class metalock) object slotd)
  (if (eq (type-of slotd) 'special-slot)
      (call-next-method)
      (let* ((name (c2mop:slot-definition-name slotd))
             (lock (cdr (assoc name (slot-value object 'slot-locks)))))
        (write-with-rw-lock (lock)
          (call-next-method)))))

(defmethod c2mop:slot-boundp-using-class :around ((class metalock) object slotd)
  (if (eq (type-of slotd) 'special-slot)
      (call-next-method)
      (let* ((name (c2mop:slot-definition-name slotd))
             (lock (cdr (assoc name (slot-value object 'slot-locks)))))
        (read-with-rw-lock (lock)
          (call-next-method)))))

(defmethod c2mop:slot-makunbound-using-class :around ((class metalock) object slotd)
  (if (eq (type-of slotd) 'special-slot)
      (call-next-method)
      (let* ((name (c2mop:slot-definition-name slotd))
             (lock (cdr (assoc name (slot-value object 'slot-locks)))))
        (write-with-rw-lock (lock)
          (call-next-method)))))

(defun slot-names-to-lock-alist (slot-names)
  (check-type slot-names list)
  (mapcar (lambda (name)
            (cons name (make-instance 'reader-writer-lock)))
          slot-names))

(defmethod c2mop:compute-slots ((class metalock))
  (let* ((normal-slots (call-next-method))
         (slot-names (mapcar #'c2mop:slot-definition-name normal-slots))
         (slot-names-lock-alist (slot-names-to-lock-alist slot-names)))
    (cons 
     (make-instance 'special-slot 
                    :name 'slot-locks
                    :initform `',slot-names-lock-alist
                    :initfunction #'(lambda () slot-names-lock-alist))
     normal-slots)))

