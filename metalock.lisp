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
        (bt:with-lock-held (lock)
          (print "reading. lock held")
          (let ((val (call-next-method)))
            (print "reading. lock dropped")
            val)))))

(defmethod (setf c2mop:slot-value-using-class) :around (new-value (class metalock) object slotd)
  (if (eq (type-of slotd) 'special-slot)
      (call-next-method)
      (let* ((name (c2mop:slot-definition-name slotd))
             (lock (cdr (assoc name (slot-value object 'slot-locks)))))
        (bt:with-lock-held (lock)
          (print "setting. lock held")
          (let ((val (call-next-method)))
            (print "setting. lock dropped")
            val)))))

(defmethod c2mop:slot-boundp-using-class :around ((class metalock) object slotd)
  (if (eq (type-of slotd) 'special-slot)
      (call-next-method)
      (let* ((name (c2mop:slot-definition-name slotd))
             (lock (cdr (assoc name (slot-value object 'slot-locks)))))
        (bt:with-lock-held (lock)
          (print "boundp. lock held")
          (let ((val (call-next-method)))
            (print "boundp. lock dropped")
            val)))))

(defmethod c2mop:slot-makunbound-using-class :around ((class metalock) object slotd)
  (if (eq (type-of slotd) 'special-slot)
      (call-next-method)
      (let* ((name (c2mop:slot-definition-name slotd))
             (lock (cdr (assoc name (slot-value object 'slot-locks)))))
        (bt:with-lock-held (lock)
          (print "making-unbound. lock held")
          (let ((val (call-next-method)))
            (print "making-unbound. lock dropped")
            val)))))

(defun slot-names-to-lock-alist (slot-names)
  (check-type slot-names list)
  (mapcar (lambda (name)
            (cons name (bt:make-lock)))
          slot-names))

(defmethod c2mop:compute-slots ((class metalock))
  (let* ((normal-slots (call-next-method))
         (slot-names (mapcar #'c2mop:slot-definition-name normal-slots))
         (instance-lock (list (list :held nil) (list :lock (bt:make-lock))))
         (slot-names-lock-alist (slot-names-to-lock-alist slot-names)))
    (append 
     (list (make-instance 'special-slot 
                          :name 'slot-locks
                          :initform `',slot-names-lock-alist
                          :initfunction #'(lambda () slot-names-lock-alist))
           (make-instance 'special-slot 
                          :name 'instance-lock
                          :initform `',instance-lock
                          :initfunction #'(lambda () instance-lock)))
     normal-slots)))

(defmethod globally-locked-p ((instance metalock))
  (second (assoc :held (slot-value instance 'instance-lock))))

(defmethod set-global-lock ((instance metalock))
  "Given an INSTANCE of a metalock class, sets the instance to globally locked. This should only 
be used when grabbing the instance lock."
  

  (defmacro synchronise ((metalocked-object) &body)
    "Given an instance of a class thats metaclass is a 'metalock'. This macro will guarantee that 
execution of body happens in a way that only one thread can modify the object at one time"
    
    )
  

  (defclass locked-object ()
    ((arr :initform "oof"
          :accessor arr-access)
     (name
      :accessor name))
    (:metaclass metalock))

