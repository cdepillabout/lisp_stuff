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


;; Another binding form is a variant of LET, LET*. The difference is that in a
;; LET, the variable names can be used only in the body of the LET--the part of
;; the LET  after the variables list--but in a LET*, the initial value forms
;; for each variable can refer to variables introduced earlier in the variables
;; list. Thus, you can write the following:
(let* ((x 10)
       (y (+ x 10)))
  (list x y))
;; but not this:
;(let ((x 10)
;      (y (+ x 10)))
;  (list x y))


;; you can capture the closure created by the previous expression in a global
;; variable like this:
(defparameter *my-cool-fn*
  (let ((count 0))
	#'(lambda ()
		(setf count (1+ count)))))
;; Then each time you invoke it, the value of count will increase by one.
;CL-USER> (funcall *fn*) ==> 1
;CL-USER> (funcall *fn*) ==> 2
;CL-USER> (funcall *fn*) ==> 3

;; defparameter and defvar establish a name as a dynamic variable.
;; defparameter unconditionally assigns the an initial-value to the dynamic
;; variable. defvar, by contrast, assigns an initial-value (if supplied) to
;; a dynamic variable only if the name is not already bound. 
(defvar a 3)        ; ==> A
a                   ; ==> 3
(defvar a 5)        ; ==> A
a                   ; ==> 3
(defparameter a 5)  ; ==> A
a                   ; ==> 5


;; This shows how dynamic and lexical scope works.
(defvar *x* 10)
(defun lalalax () (format t "X: ~d~%" *x*))
(lalalax) ; ==> 10
(let ((*x* 20))
  (lalalax)) ; ==> 20
(lalalax) ; ==> 10

(defun barbar ()
  (lalalax)
  (let ((*x* 20)) (lalalax))
  (lalalax))

(barbar) ; ==> X: 10, X: 20, X: 10

(defun lalalax ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))

;; The middle call doesn't see the global binding because of the LET.
(barbar) ; ==>
; Before assignment X: 10
; After assignment  X: 11
; Before assignment X: 20
; After assignment  X: 21
; Before assignment X: 11
; After assignment  X: 12

;; If you always name global variables according to the * naming convention,
;; you'll never accidentally use a dynamic binding where you intend to
;; establish a lexical binding.


;; The basic form of DEFCONSTANT is like DEFPARAMETER.
;(defconstant name initial-value-form [ documentation-string ])
;; DEFCONSTANT has a global effect on the name used--thereafter the name
;; can be used only to refer to the constant; it can't be used as a function
;; parameter or rebound with any other binding form. Thus, many Lisp
;; programmers follow a naming convention of using names starting and ending
;; with + for constants.


;; Set a binding to a new value
(defvar hohoho 24)
hohoho ; ==> 24
(setf hohoho 5000)
hohoho ; ==> 5000

; instead of this
;(setf x 1)
;(setf y 2)
; you can write this:
;(setf x 1 y 2)

;; you can also do something like this, which sets x and y to be the same 
;; random value
;(setf x (setf y (random 10)))

;; Here is how setf works with normal values, arrays, hashes, and fields
; Simple variable:    (setf x 10) 
; Array:              (setf (aref a 0) 10)
; Hash table:         (setf (gethash 'key hash) 10)
; Slot named 'field': (setf (field o) 10)


;; Quick incrementers and decrementers
; (incf x)    === (setf x (+ x 1))
; (decf x)    === (setf x (- x 1))
; (incf x 10) === (setf x (+ x 10))


;; when and unless macros
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))
(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

(when (< 1 2)
  (print "hello")
  (print "bye")
  3)

;; dolist and dotimes -- pretty basic looping constructs		 
(dolist (x '(1 2 3)) (print x))
(dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))
(dotimes (i 4) (print i))

;; basic do template:
; (do (variable-definition*)
;     (end-test-form result-form*)
;   statement*)

;; variable definition template in do form:
; (var init-form step-form)
;; If you leave out step-form, then the variable keeps the same
;; value unless you explicitly change it.

;; At the beginning of each iteration, after all the loop variables have
;; been given their new values, the end-test-form is evaluated. As long
;; as it evaluates to NIL, the iteration proceeds, evaluating the statements
;; in order. When the end-test-form evaluates to true, the result-forms
;; are evaluated, and the value of the last result form is returned as the
;; value of the DO expression.

;; Print fibonacci numbers, returning the 11th fibonacci number
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur)
  (print cur))


;; To print a character
#\l ; ==> prints the character 'l'

;; Make a list of all the lower case characters
(loop for i from 97 until (eql (code-char i) #\{) collect (code-char i))


;; Macro for doprimes, which is a loop that executes the body once
;; for each prime number.

; helper functions
(defun primep (number)
  "Test whether or not a number is prime."
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "Find the next prime number equal to or greater than number."
  (loop for n from number when (primep n) return n))

; how we should call the macro
(do-primes (p 0 19)
  (format t "~d " p))

; you should use &body for this because it makes more sense
(defmacro do-primes (p start end &body body)
  `(do ((,p (next-prime ,start) (next-prime (1+ ,p))))
	   ((> ,p ,end) ,p) 
	 ,@body))

(defmacro do-primes2 (listargs &body body)
  (let ((p (car listargs))
		(start (cadr listargs))
		(end (caddr listargs)))
	`(do ((,p (next-prime ,start) (next-prime (1+ ,p))))
		 ((> ,p ,end) ,p) 
	   ,@body)))
	
;; Destructuring parameter lists.
(defmacro do-primes3 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))


;; But this leaks.  end will be evaluated multiple times.
;; Here is how to fix it:
(defmacro do-primes-4 ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

;; But this also doesn't work.  end will be evaluated before
;; start.  So this is how you can fix it:
(defmacro do-primes-5 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))
;; This also doesn't work correctly!! It will fail on a call like this:
;(do-primes-5 (ending-value 0 10)
;  (print ending-value))
;; It will macro expand to this:
;(do ((ending-value (next-prime 0) (next-prime (1+ ending-value)))
;     (ending-value 10))
;    ((> ending-value ending-value))
;  (print ending-value))
;; This is not correct because ending-value is used in two different places

;; The way to fix this is with gensym
(defmacro do-primes-6 ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))
;; ending-value-name is a variable whose value is the name of another variable


;;; Rules of thumb for writing leakless macros:
;;; 1) Unless there's a particular reason to do otherwise, include any
;;;    subforms in the expansion in positions that will be evaluated in
;;;    the same order as the subforms appear in the macro call.
;;; 2) Unless there's a particular reason to do otherwise, make sure subforms
;;;    are evaluated only once by creating a variable in the expansion to hold
;;;    the value of evaluating the argument form and then using that variable
;;;    anywhere else the value is needed in the expansion.
;;; 3) Use GENSYM at macro expansion time to create variable names used in the
;;;    expansion.

;; this is my version that is just a little different
(defmacro do-primes-7 ((var start end) &body body)
  (let ((starting-value-name (gensym))
		(ending-value-name (gensym)))
	`(let ((,starting-value-name ,start)
		   (,ending-value-name ,end))
	   (do ((,var (next-prime ,starting-value-name) (next-prime (1+ ,var))))
		   ((> ,var ,ending-value-name))
		 ,@body))))


;; It would be nice to write a macro that could do the gensym stuff for us.
;; Note how you can use a comma to interpolate the value of the LOOP expression.
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; So now you can write do-primes like this:
(defmacro do-primes-8 ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))


;; Another classic macro-writing macro is once-only, which is used to generate
;; code that evaluates certain macro arguments once only and in a particular
;; order.
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
	   `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
		  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
				,@body)))))

;; Using once-only, you could write do-primes almost as simply as the original
;; leaky version, like this:
(defmacro do-primes-11 ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))
