
;; Both CAR and CDR are also SETFable places--given an existing cons
;; cell, it's possible to assign a new value to either of its values.

(defparameter *cons* (cons 1 2))
*cons* ; ==> (1 . 2)
(setf (car *cons*) 10) ; ==> 10
*cons* ; ==> (10 . 2)
(setf (cdr *cons*) 20) ; ==> 20
*cons* ; ==> (10 . 20)

; (cons 1 nil) == (list 1)
; (first *list*) == (car *list*)
; (rest *list*) == (cdr *list*)
