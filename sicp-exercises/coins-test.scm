;; Tail recursive version of the change counter from section 1.2.2.
;; I could have probably made this much more "scheme-like" by making it 
;; more functional, but I currently don't know the commands/syntax/etc.  So, I 
;; keep going in SICP!

(define (reference-worth index)
  (cond ((= index 0) 5)
	((= index 1) 10)
	((= index 2) 25)
	((= index 3) 50)))

(define (total-possible index)
  (cond ((= index 0) 20)
	((= index 1) 10)
	((= index 2) 4)
	((= index 3) 2)))

(define (total-sum lst)
  (+ (* (list-ref lst 0) (reference-worth 0))
     (* (list-ref lst 1) (reference-worth 1))
     (* (list-ref lst 2) (reference-worth 2))
     (* (list-ref lst 3) (reference-worth 3))))

(define (can-add-another index lst top-sum-amount)
  (and (< (list-ref lst index) (total-possible index))
       (<= (+ (total-sum lst)
	      (reference-worth index))
	   top-sum-amount)))


(define (increment-reference index lst)
  (list (if (= index 0)
	    (+ (list-ref lst 0) 1)
	    (list-ref lst 0))
	(if (= index 1)
	    (+ (list-ref lst 1) 1)
	    (list-ref lst 1))
	(if (= index 2)
	    (+ (list-ref lst 2) 1)
	    (list-ref lst 2))
	(if (= index 3)
	    (+ (list-ref lst 3) 1)
	    (list-ref lst 3))))

(define (zero-previous lst index)
  (list (if (> index 0)
	    0
	    (list-ref lst 0))
	(if (> index 1)
	    0
	    (list-ref lst 1))
	(if (> index 2)
	    0
	    (list-ref lst 2))
	(list-ref lst 3)))
  

(define (first-non-full-index lst top-sum-amount)
  (cond ((can-add-another 0 lst top-sum-amount) 0)
	((can-add-another 1 (zero-previous lst 1) top-sum-amount) 1)
	((can-add-another 2 (zero-previous lst 2) top-sum-amount) 2)
	((can-add-another 3 (zero-previous lst 3) top-sum-amount) 3)
	(else 4)))
	


(define (my-cc-2 lst counter top-sum-amount)
  (let ((not-full-indx (first-non-full-index lst top-sum-amount)))
    (let ((zerod-list (zero-previous lst not-full-indx)))
      (begin (display
	      (list 'not-full-indx not-full-indx
		    'counter counter
		    'list lst 'zerod-list zerod-list))
	     (display "\n")
	     (if (> not-full-indx 3)
		 (list lst (+ 1 counter))
		 (my-cc-2 (increment-reference not-full-indx zerod-list)
			  (+ 1 counter) top-sum-amount))))))


;(my-cc-2 '(0 0 0 0) 0 100)


