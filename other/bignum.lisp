;;;; This is just a test for dealing with bignums (despite
;;;; the fact that CL will do this for us automatically)

(defparameter *my-debug* t)

(defmacro my-debug (function &rest rest)
  `(if *my-debug*
	   (,function ,@rest)
	   nil))

(defun halve-once (list &optional old-remainder)
  (let* ((num (first list))
		 (num-plus-remainder (if old-remainder (+ num 10) num))
		 (multivalues (multiple-value-list (truncate (float (/ num-plus-remainder 2)))))
		 (result-value (car multivalues))
		 (new-remainder (not (zerop (cadr multivalues)))))
	(my-debug format t "list: ~a, len: ~a, num-plus-remainder: ~a, result-value: ~a, old-remainder: ~a, new-remainder: ~a~%"
			list (length list) num-plus-remainder result-value old-remainder new-remainder)
	(if (= (length list) 1)
		(values (list result-value) new-remainder)
		(let* ((multivalues (multiple-value-list (halve-once (cdr list) new-remainder)))
			   (return-list (car multivalues))
			   (remainder (cadr multivalues)))
		  (values (cons result-value return-list) remainder)))))
		  

(defun create-binary-list (list &optional consumer)
  "Take an arbitrary number represented in a list and
   return a list containing the number represented in 
   binary.  For instance, to get a binary list for the number 12,
   you would pass in the list '(1 2) and get back the list 
   '(1 1 0 0)."
  ;(let ((all-zeros (if (find-if-not #'zerop list) nil t)))
  (let ((all-zeros (every #'zerop list)))
	(cond
	 ;; we have a binary string started that we've been building, so
	 ;; we just return it.
	 ((and consumer all-zeros) consumer)
	 ;; we don't have a binary string started in consumer, but
	 ;; we are all zeros, so someone is trying to just create the binary
	 ;; list for (0).  Just return (0).
	 ((and (not consumer) all-zeros) '(0))
	 ;; otherwise, we start building the binary list in consumer
	 (t (let* ((multivalues (multiple-value-list (halve-once list)))
			   (new-list (car multivalues))
			   (result-binary (if (cadr multivalues) 1 0)))
		  (create-binary-list new-list (cons result-binary consumer)))))))
	  
	  
(defun string-to-bignum (string)
  "Takes a number in string form and converts it to a bignum
   (which is just a list of 1's and 0's representing a bignum)."
  (let ((*my-debug* nil))
	(create-binary-list
	 (mapcar #'(lambda (num) (parse-integer (string num)))
			 (coerce string 'list)))))


(defun num-to-bignum (num)
  "Takes a number and converts it to a bignum."
  (string-to-bignum (write-to-string num)))

(defun double-just-one (num remainder)
  "This just takes one number and doubles it and adds the remainder.
   It returns the doubled number mod 10, and then
   true if it was over 10, or nil if it was not over 10"
  (let ((new-value (+ (* num 2) remainder)))
	(if (>= new-value 10)
		(values (- new-value 10) t)
		(values new-value nil))))
		

(defun double-once (num-list remainder &optional (i 0))
  "This doubles a number represented in num-list for, so
   this a reversed number list (for example, the number 510 is 
   represented as '(0 1 5)). The remainder is an extra value to
   add, it should come from the binary string (so it's either 1 or 0).
   The doubled num-list is returned and it is also reversed."
  (my-debug format t "num-list: ~a, remainder: ~a, i: ~a~%"
			num-list remainder i)
  (if (>= i (length num-list))
	  (if (= 1 remainder) '(1) '())
	  (let* ((multivalues (multiple-value-list (double-just-one (nth i num-list) remainder)))
			 (new-value (car multivalues))
			 (new-remainder (if (cadr multivalues) 1 0)))
		(my-debug format t "new-value: ~a, new-remainder: ~a~%"
				  new-value new-remainder)
		(cons new-value (double-once num-list new-remainder (1+ i))))))

(defun create-bignum-list (binary-list &optional (num-list '(0)))
  (if (not binary-list)
	  num-list
	  (create-bignum-list (cdr binary-list)
						  (double-once num-list (car binary-list)))))


(defun bignum-to-string (bignum)
  "Takes a bignum and converts it to a string of numbers."
  (let ((*my-debug* nil))
	(reduce #'(lambda (x y) (concatenate 'string x (write-to-string y)))
			(reverse (create-bignum-list bignum))
			:initial-value "")))
   			 
