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

(defun begin-read (reader-writer-lock)
  (with-accessors ((g g-lock)
                   (ww writers-waiting)
                   (c-var condition-var)
                   (ra readers-active))
      reader-writer-lock
    (bt:with-lock-held (g)
      (while-loop (> ww 0)
        (bt:condition-wait c-var g))
      (incf ra))))

(defun end-read (reader-writer-lock)
  (with-accessors ((g g-lock)
                   (c-var condition-var)
                   (ra readers-active))
      reader-writer-lock
    (bt:with-lock-held (g)
      (unless (zerop ra)
        (decf ra))
      (when (zerop ra)
        (bt:condition-notify c-var)))))

(defun begin-write (reader-writer-lock)
  (with-accessors ((g g-lock)
                   (ra readers-active)
                   (ww writers-waiting)
                   (c-var condition-var)
                   (aw active-writer-p))
      reader-writer-lock
    (bt:with-lock-held (g)
      (incf ww)
      (while-loop (or (> ra 0) aw)
        (bt:condition-notify c-var))
      (decf ww)
      (setf aw t))))

(defun end-write (reader-writer-lock)
  (with-accessors ((g g-lock)
                   (c-var condition-var)
                   (aw active-writer-p))
      reader-writer-lock
    (bt:with-lock-held (g)
      (setf aw nil)
      (bt:condition-notify c-var))))

(defmacro read-with-rw-lock ((lock) &body body)
  `(unwind-protect
        (prog2 (begin-read ,lock)
            (progn ,@body)
          (end-read ,lock))
     (end-read ,lock)))

(defmacro write-with-rw-lock ((lock) &body body)
  `(unwind-protect
        (prog2 (begin-write ,lock)
            (progn ,@body)
          (end-write ,lock))
     (end-read ,lock)))
