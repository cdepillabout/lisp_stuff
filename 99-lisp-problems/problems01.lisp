;;;; These are solutions to problems from
;;;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html


;; Problem 01
;; Find the last box of a list.
(defun my-last (list)
  (cond ((cdr list)
		 (my-last (cdr list)))
		(t list)))

(my-last '(a b c d)) ; ==> (D)


;; Problem 02
;; Find the last but one box of a list.
(defun my-x-last-helper (list func)
  (cond ((funcall func list)
		 (my-x-last-helper (cdr list) func))
		(t list)))

(defun my-but-last (list)
  (my-x-last-helper list 'cddr))

(my-but-last '(1 2 3)) ; ==> (2 3)


;; Problem 03
;; Find the K'th element of a list.
(defun element-at (list i)
  (cond ((< i 1) nil)
		((not list) nil)
		((= 1 i) (car list))
		(t (element-at (cdr list) (- i 1)))))
		 
(element-at '(a b c d e) 4) ; ==> D


;; Problem 04
;; Find number of elements of a list
(defun num-of-elements (list &optional (i 0))
  (if (not list) 
	  i
	  (num-of-elements (cdr list) (+ i 1)))) 

(num-of-elements '(a z c d)) ; ==> 4


;; Problem 05
;; Reverse a list
(defun my-reverse-list (list)
  (if (not list)
	  nil
	  (append (my-reverse-list (cdr list)) (list (car list)))))

(defun my-reverse-list-3 (list &optional (i 1) (rest nil))
  (if (or (< i 1) (> i (num-of-elements list)))
	  rest
	  (my-reverse-list-3 list (+ i 1)
						 (cons (element-at list i) rest))))

(my-reverse-list-3 '(1 2 3)) ; ==> (3 2 1)


;; Problem 06
;; Find out whether a list is a palindrome
(defun is-palindrome (list)
  (equal list (my-reverse-list list)))

(is-palindrome '(3 2 4 2 3)) ; ==> T


