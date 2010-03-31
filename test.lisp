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

;(backwards (3 2 +))

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
(defun foo-key (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))

(foo-key :apple 3 :charlie t)


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


;; following function uses nested loops to find the first pair of
;; numbers, each less than 10, whose product is greater than the argument,
;; and it uses RETURN-FROM to return the pair as soon as it finds it
(defun foo3 (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo3 (list i j))))))


(function foo3) ; this is the same as #'foo3

(funcall #'foo3 55) ; this is exactly the same as (foo3 55) 

;; accepts a function object as an argument and plots a simple ASCII-art
;; histogram of the values returned by the argument function when it's
;; invoked on the values from min to max, stepping by step.
(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))
; You can call it like this: (plot #'exp 0 4 1/2)

;; But imagine you wanted to call it with a list.
;; You can do it like this:
;; (apply #'plot '(exp 0 4 1/2))
;; You can even do something like this:
;; (apply #'plot #'exp (0 4 1/2))
;; (apply #'plot #'exp 0 (4 1/2))


;; lambda
(funcall #'(lambda (x y) (+ x y)) 1 1000)