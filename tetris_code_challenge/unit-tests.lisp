(in-package :unit-tests)

;(deftest test-+ ()
;  (check
;	(= (+ 1 2) 3)
;	(= (+ 2 2) 4)))
;
;(deftest test-- ()
;  (check
;	(= (- 1 1) 0)
;	(= (- 2 3) -1)))

;(deftest test-arithmetic ()
;  (check
;	(test-+)
;	(test--)))

;(deftest test-math ()
;  (test-arithmetic))

;(defun test-everything ()
;  (if (test-math)
;	  (format t "Everything Passed~%")))

;;; Element Manipulation Tests

(deftest test-get-set-element ()
  (check
	(equal (get-element 0 0 (set-element *j-symbol* 0 0
										 (create-play-space 2 3)))
		   *j-symbol*)
	(equal (get-element 2 1 (set-element *empty-symbol* 2 1 *j-piece*))
		   *empty-symbol*)))

(deftest test-element-manip()
  (check 
	(test-get-set-element)))


(defun test-everything ()
  (check 
	(test-element-manip)))
  
