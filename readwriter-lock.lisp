(in-package :metalock)

(defmacro while-loop (test &body body)
  `(loop :while ,test
         :do ,@body))


;;;;https://en.wikipedia.org/wiki/Readers%E2%80%93writer_lock#Using_a_condition_variable_and_a_mutex
;;;;implementation of a read-writer-lock using a conditional variable and a mutex

(defclass reader-writer-lock ()
  ((condition-var
    :accessor condition-var
    :initform (bt:make-condition-variable))
   (g-lock
    :accessor g-lock
    :type bt:lock
    :initform (bt:make-lock))
   (readers-active
    :accessor readers-active
    :type integer
    :initform 0)
   (writers-waiting
    :accessor writers-waiting
    :type integer
    :initform 0)
   (active-writer-p
    :accessor active-writer-p 
    :type boolean
    :initform nil)))

(defmacro write-with-rw-lock ((reader-writer-lock) &body body)
  (let ((retval (gensym))
        (lock (gensym)))
    `(let ((,retval nil)
           (,lock ,reader-writer-lock))
       (with-accessors ((g g-lock)
                        (ra readers-active)
                        (ww writers-waiting)
                        (c-var condition-var))
           ,lock
         (bt:with-lock-held (g)
           (unwind-protect
                (incf ww)
             (unwind-protect
                  (progn
                    (while-loop (> ra 0)
                                (bt:condition-wait c-var g))
                    (setf ,retval (locally ,@body)))
               (decf ww)
               (bt:condition-notify c-var)))))
       ,retval)))

(defmacro read-with-rw-lock ((reader-writer-lock) &body body)
  (let ((retval (gensym))
        (lock (gensym)))
    `(let ((,retval ,nil)
           (,lock ,reader-writer-lock))
       (with-accessors ((g g-lock)
                        (ww writers-waiting)
                        (c-var condition-var)
                        (ra readers-active))
           ,lock
         (bt:with-lock-held (g)
           (unwind-protect 
                (while-loop (> ww 0)
                  (bt:condition-wait c-var g))
             (unwind-protect
                  (progn (incf ra)
                         (setf ,retval (locally ,@body)))
               (decf ra)
               (when (zerop ra)
                 (bt:condition-notify c-var))))))
       ,retval)))
