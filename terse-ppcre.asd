;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage #:terse-ppcre-asd
  (:use :cl :asdf))

(in-package :terse-ppcre-asd)


(defsystem terse-ppcre
  :name "terse-ppcre"
  :description "Shortcut for cl-ppcre regex parse-trees."
  :version "0.01"
  :author "Aaron Sokoloski <asokoloski@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre)
  :components ((:file "packages")
               (:file "terse-ppcre" :depends-on ("packages")))
  :in-order-to ((test-op (load-op terse-ppcre-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :fiveam)
                             (intern "ALL"
                                     :terse-ppcre-tests)))
)


(defsystem terse-ppcre-tests
  :components ((:file "tests"))
  :depends-on (:terse-ppcre :fiveam))


;; all test systems
(defmethod operation-done-p ((o test-op) (c system))
  (values nil))

(defmethod operation-done-p ((o test-op)
                             (c (eql (find-system 'terse-ppcre-tests))))
  (values nil))


(defun cldoc-filter (d)
  (if (or (typep d 'cldoc::lambda-descriptor)
          (typep d 'cldoc::param-descriptor)
          )
      (multiple-value-bind (sym type) (find-symbol (string-upcase (cldoc::name d)) (cldoc::dpackage d))
        (declare (ignore sym))
        (not (eq type :external)))
      nil))
