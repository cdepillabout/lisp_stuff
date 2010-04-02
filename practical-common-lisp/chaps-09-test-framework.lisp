;;;; This is making a unit testing framework.


(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

;; This is a naive approach, not using macros.
(defun test-+-basic ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;; So we can write check like this.
(defmacro check-basic (form)
  `(report-result ,form ',form))

;; Now test will be a little better, but still not that good.
;; We can still do better.
(defun test-+-okay ()
  (check-basic (= (+ 1 2) 3))
  (check-basic (= (+ 1 2 3) 6))
  (check-basic (= (+ -1 -3) -4)))

;; This definition uses a common macro idiom of wrapping a PROGN
;; around a series of forms in order to turn them into a single form.
;; Notice also how you can use ,@ to splice in the result of an
;; expression that returns a list of expressions that are themselves
;; generated with a backquote template.
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))


;; This uses a similar method to print the argument and 
;; the result of evaluating the argument.
(defmacro result-of (arg)
  `(format t "~a: ~a~%" ',arg ,arg))


;;;; Now here is a better version that will tell you 
;;;; whether or not all the tests pass.

;; This actually returns the result
(defun new-report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;; This was from the last section.
(defmacro with-gensyms ((&rest names) &body body)
   ;; this line could aslo be written like this:
 ;`(let (,@(loop for n in names collect `(,n (gensym))))
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; This will be used in our new check macro to show what the final 
;; result was.  This will replace the progn.
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

;; New check using combine-results
(defmacro check-new (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(new-report-result ,f ',f))))

(defun test-+ ()
  (check-new
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(defun test-* ()
  (check-new
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(defun test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))

;; If you create a dynamic variable that each test function
;; binds to the name of the function before calling check,
;; then report-result can use it without check having to know
;; anything about it.
(defvar *test-name* nil)

;; For the name to be reported properly, you
;; need to change the two test functions.
(defun test-+-2 ()
  (let ((*test-name* 'test-+))
    (check-new
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))

(defun test-*-2 ()
  (let ((*test-name* 'test-*-2))
    (check-new
      (= (* 2 2) 4)
      (= (* 3 5) 15))))

;; Because the pattern you're trying to capture is a DEFUN 
;; plus some boilerplate code, you need to write a macro
;; that will expand into a DEFUN. You'll then use this macro, 
;; instead of a plain DEFUN to define test functions, so
;; it makes sense to call it deftest.
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
	  (format t "~%")   ; a little bit of spacing is good
      ,@body
	  (format t "~%"))))

;; With this macro you can rewrite test-+ as follows:
(deftest test-+-new-3 ()
  (check-new
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-*-new-3 ()
  (check-new
	(= 1 1)
	(= (* 1 1) 1)
	(= 5 4)))

;; this will show multiple levels of using *test-name*
(deftest test-arithmetic-new ()
  (check-new
	(test-+-new-3)
	(test-*-new-3)))

(deftest test-math ()
  (check-new
	(test-arithmetic-new)))

;; This is my attempt to get rid of having to write
;; check-new
(defmacro my-deftest (name parameters &body body)
  `(defun ,name ,parameters
	 (let ((*test-name* (append *test-name* (list ',name))))
	   (format t "~%")   ; a little bit of spacing is good
	   (check-new 
		 ,@body)
	   (format t "~%"))))

(my-deftest test-+-new-4 ()
  (= (+ 1 2) 3)
  (= (+ 1 2 3) 6)
  (= (+ -1 -3) -4))

(my-deftest test-*-new-4 ()
  (= 1 1)
  (= (* 1 1) 1)
  (= 5 4))

;; this will show multiple levels of using *test-name*
(my-deftest test-arithmetic-new-4 ()
  (test-+-new-4)
  (test-*-new-4))

(my-deftest test-math-4 ()
  (test-arithmetic-new-4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Here is the final code produced from this chapter:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-name-final* nil)

(defmacro deftest-final (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name-final* (append *test-name-final* (list ',name))))
      ,@body)))

(defmacro check-final (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results-final
    ,@(loop for f in forms collect `(report-result-final ,f ',f))))

(defmacro combine-results-final (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result-final (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)
