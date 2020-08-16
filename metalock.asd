;;;; metalock.asd

(asdf:defsystem #:metalock
  :description "A metaclass that makes building parallel systems easier by providing each slot within a class a lock which is grabbed automatically."
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:closer-mop
               #:bordeaux-threads)
  :components ((:file "package")
               (:file "readwriter-lock")
               (:file "metalock")))
