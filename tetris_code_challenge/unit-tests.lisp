(in-package :unit-test)

(deftest test-+ ()
  (check
	(= (+ 1 2) 3)
	(= (+ 2 2) 4)))

(deftest test-- ()
  (check
	(= (- 1 1) 0)
	(= (- 2 3) -1)))

(deftest test-arithmetic ()
  (check
	(test-+)
	(test--)))

(deftest test-math ()
  (test-arithmetic))

(test-math)
