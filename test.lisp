(defun my-reverse (list)
  (cond ((atom list) list)
		((atom (car list))
		 (append (my-reverse (cdr list))
				 (list (car list))))
		(t (append (my-reverse (cdr list))
				   (list (my-reverse (car list)))))))

(defun my-reverse2 (list)
  (if (atom list)
	  list
	  (append (my-reverse2 (cdr list))
			  (list (if (atom (car list))
						(car list)
						(my-reverse2 (car list)))))))


(defmacro backwards (expr) (my-reverse2 expr))

(backwards (3 2 +))

`(1 2 (+ 1 2)) ; ==> (1 2 (+ 1 2))
`(1 2 ,(+ 1 2)); ==> (1 2 3)

`(and ,(list 1 2 3));    ==> (AND (1 2 3))
`(and ,@(list 1 2 3));   ==> (AND 1 2 3)
`(and ,@(list 1 2 3) 4); ==> (AND 1 2 3 4)

(defmacro my-when (test &rest body)
  `(if ,test 
     (progn ,@body)))

(my-when (eq 1 1) (print "hello"))

(macroexpand-1 '(my-when t (print "hello")))





;;;; Four semicolons are used for a file header comment.

;;; A comment with three semicolons will usually be a paragraph
;;; comment that applies to a large section of code that follows,

(defun foo (x)
  (dotimes (i x)
    ;; Two semicolons indicate this comment applies to the code
    ;; that follows. Note that this comment is indented the same
    ;; as the code that follows.
    (print "hello")
    (print i)              ; this comment applies to this line only
    (print "bye")            ; and this is for this line
    (+ i 1)))


(defun my-add (a &optional (b a))
  "This takes one, optionally two params.  It assigns the b variable to be
   a if the caller doesn't provide b.  So, by default it doubles a."
  (+ a b))
;; You can only use parameters defined earlier in the default value forms.
;; So (b a) is possible like above, but the other way around would not be valid.



;; the keyword the caller uses to specify the parameter to be different from
;; the name of the actual parameter, you can replace the parameter name with
;; another list containing the keyword to use when calling the function and
;; the name to be used for the parameter.
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(foo :apple 3 :charlie t)


;; The problem is that if a caller doesn't supply values for
;; all the optional parameters, then those parameters will eat
;; up the keywords and values intended for the keyword
;; parameters. For instance, this function unwisely mixes &optional
;; and &key  parameters:
(defun foo1 (x &optional y &key z) (list x y z))
;If called like this, it works fine:
(foo1 1 2 :z 3) ; ==> (1 2 3)
; And this is also fine:
(foo 1) ; ==> (1 nil nil)
;But this will signal an error:
;(foo 1 :z 3) ==> ERROR
