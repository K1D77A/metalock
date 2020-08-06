;;;; metalock.lisp

(in-package #:metalock)

(defclass metalock (c2mop:standard-class)
  ((locks :type list :accessor locks)))

(defmethod c2mop:validate-superclass ((class metalock) (metaclass standard-class))
  t)
(defmethod c2mop:validate-superclass ((class standard-class) (metaclass metalock))
  t)

(defclass locked-object ()
  ((arr :initform "oof"
        :accessor arr-access))
  (:metaclass metalock))

(defparameter *lock* (bt:make-lock))


;; (defmethod c2mop:slot-value-using-class :around ((class metalock) object slotd)
;;   (bt:with-lock-held (*lock*)
;;     (print "lock held")
;;     (let ((val (call-next-method)))
;;       (print "lock dropped")
;;       val)))

(defun slot-names-to-lock-alist (slot-names)
  (check-type slot-names list)
  (mapcar (lambda (name)
            (cons name (bt:make-lock)))
          slot-names))

(defmethod c2mop:effective-slot-definition-class ((class metalock) &rest _)
  (declare (ignore _))
  (find-class 'c2mop:effective-slot-definition ))

(defmethod c2mop:direct-slot-definition-class ((class metalock) &rest _)
  (declare (ignore _))
  (find-class 'c2mop:standard-direct-slot-definition))

(defmethod c2mop:compute-slots ((class metalock))
  (let* ((normal-slots (call-next-method))
         (slot-names (mapcar #'c2mop:slot-definition-name normal-slots))
         (slot-names-lock-alist (slot-names-to-lock-alist slot-names)))
    (cons
     (make-instance 'c2mop:direct-slot-definition
                    :name 'slot-locks
                    :initform `',slot-names-lock-alist
                    :initfunction #'(lambda () slot-names-lock-alist))
     normal-slots)))


