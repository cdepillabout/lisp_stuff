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


;; Problem 07
;; Flatten a nested list structure.
;; Transform a list, possibly holding lists as elements into a
;; `flat' list by replacing each list with its elements (recursively).
(defun my-flatten (list)
  (let ((firstval (car list))
		(remaining (cdr list)))
	(cond ((not list) nil)
		  ((atom firstval)
		   (append (list firstval)
				   (my-flatten remaining)))
		  ((listp firstval)
		   (append (my-flatten firstval) (my-flatten remaining))))))
		  


(my-flatten '(a (b (c d) e))) ; ==> (A B C D E)
(my-flatten '(a (b d (z y x)) c (e f)))


;; Problem 08
;; Eliminate consecutive duplicates of list elements.
;; If a list contains repeated elements they should be replaced with
;; a single copy of the element. The order of the elements should not be 
;; changed.
(defun my-compress (list)
  (let ((firstval (car list))
		(remaining (cdr list)))
	(cond ((not remaining) list)
		  ((eql firstval (car remaining))
		   (my-compress (cons firstval (cdr remaining))))
		  (t (cons firstval (my-compress remaining))))))

(my-compress '(a a a a b c c a a d e e e e)) ; ==> (A B C A D E)

;; Problem 09
;; Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.
(defun pack (list &optional (sublist nil))
  (cond ((not list) (list sublist))
		
		((not sublist)
		 (pack (cdr list) (list (car list))))
		
		((equal (car sublist) (car list))
		 (pack (cdr list)
			   (cons (car list) sublist)))
		
		(t (cons sublist (pack list)))))

(pack '(a a a a b c c a a d e e e e)) ; ==> ((A A A A) (B) (C C) (A A) (D) (E E E E))


;; Problem 10
;; Run-length encoding of a list.
;; Use the result of problem P09 to implement the so-called run-length
;; encoding data compression method. Consecutive duplicates of elements 
;; are encoded as lists (N E) where N is the number of duplicates of the
;; element E.
(defun encode-helper (packedlist)
  (let ((firstelement (caar packedlist))
		(firstlength (length (car packedlist))))
	(if (not packedlist)
		nil
		(cons (list firstlength firstelement) (encode-helper (cdr packedlist))))))

(defun encode (list)
  (if (not list)
	  nil
	  (encode-helper (pack list))))
		
(encode '(a a a a b c c a a d e e e e)) ; ==> ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))


