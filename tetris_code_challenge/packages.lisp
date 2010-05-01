(in-package :cl-user)

(defpackage :unit-test-framework
  (:use :common-lisp)
  (:export :deftest
		   :check))

(defpackage :unit-tests
  (:use :common-lisp :unit-test-framework))
