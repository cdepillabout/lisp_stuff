(in-package :cl-user)

(defpackage :unit-test-framework
  (:use :common-lisp)
  (:export :deftest
		   :check))

(defpackage :unit-tests
  (:use :common-lisp :unit-test-framework :tetris-challenge))

(defpackage :tetris-challenge
  (:use :common-lisp)
  (:export :*empty-symbol* :*i-symbol* :*j-symbol* :*l-symbol*
		   :*o-symbol* :*s-symbol* :*t-symbol* :*z-symbol*
		   :*i-piece* :*j-piece* :*l-piece* :*o-piece*
		   :*s-piece* :*t-piece* :*z-piece*
		   :create-piece :print-piece
		   :rotate-90 :rotate-180 :rotate-270))
