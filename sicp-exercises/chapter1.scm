



(define (mult6 x)
  (* x 20))

(define siz 2)

(* siz 5)

(define (square x) (* x x))


(define  (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs-1 x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))


(define (abs-2 x)
  (if (< x 0)
      (- x)
      (x)))

(define (min-2 x y)
  (if (< x y)
      x
      y))

(define (min-3 x y z)
  (min-2 (min-2 x y) (min-2 x z)))

(define (max-square x y z)
  (cond ((= (min-3 x y z) x) (sum-of-squares y z))
	((= (min-3 x y z) y) (sum-of-squares x z))
	((= (min-3 x y z) z) (sum-of-squares x y))))


; Newton's method for finding square roots.
; pg. 23.
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))



(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))


; exercise 1.6.  page 25.
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

; this causes an infinite loop
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (new-sqrt-iter (improve guess x)
		     x)))

(define (proof-new-if)
  ; this evaluates only (print "no") once
  (if (= 2 3) (print "yes") (print "no"))
  ; this both the prints, and then runs (print "no") again
  (new-if (= 2 3) (print "yes") (print "no")))


; Exercise 1.7
(define (new-good-sqrt-iter prior-guess guess x)
  (if (my-good-enough? prior-guess guess)
      guess
      (new-good-sqrt-iter guess (improve guess x) x)))

(define (my-good-enough? prior-guess guess)
  (< (abs (- prior-guess guess)) 0.00001))

; Exercise 1.8
(define (cube x)
  (* x x x))

(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube-improve guess x)
  (/ (+ (/ x (* guess guess))
	(* 2 guess))
     3))

(define (cbrt-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cbrt-iter (cube-improve guess x)
		 x)))

(define (my-cbrt x)
  (cbrt-iter 1.0 x))


; Lexical scope sqrt
(define (lexical-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; factorial
(define (fact x)
  (define (fact-helper total working-number)
    (if (= working-number x)
	(* total working-number)
	(fact-helper (* total working-number)
		     (+ working-number 1))))
  (fact-helper 1 1))

; this cannot work because we can't change x, its the same everytime
; fact-helper is run.
(define (fact-2 x)
  (define (fact-helper total)
    (if (<= x 1)
	total
	(fact-helper (* total x))))
  (fact-helper 1))


; Exercise 1.10
; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

; Fibonacci sequence pg. 39
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
	b
	(fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))


; Count change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount (first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))


; Exercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	(* 2 (f (- n 2)))
	(* 3 (f (- n 3))))))

(define (f-2 n)
  (f-iter 2 1 0 (- n 3)))

(define (f-iter a b c count)
  (cond ((= count -3) c)
	((= count -2) b)
	((= count -1) a)
	(else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))

; Exercise 1.12
(define (pascal-triangle row index)
  (cond ((< row 1) "row is less than 1")
	((or (< index 0) (>= index row))
	 "index is less than 0 or greater than or equal to row")
	((= 0 index) 1)
	((= (- row 1) index) 1)
	(else (+ (pascal-triangle (- row 1) (- index 1))
		 (pascal-triangle (- row 1) index)))))


; Fast exponentiation. b ^ n.  This is faster when b is even.
(define (fast-exponent b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-exponent b (/ n 2))))
	(else (* b (fast-exponent b (- n 1))))))

; this does not work
;(define (testest #!optional a)
;  a)
  
(define (fast-iterative-exponent num pow)
  (define (fie-helper num pow a)
    (cond ((= pow 0) a)
	  ((even? pow) (fie-helper (square num) (/ pow 2) a))
	  (else (fie-helper num (- pow 1) (* a num)))))
  (fie-helper num pow 1))
