
;; write a number in hex
#xA ; => 10

;; in binary
#b1010 ; ==> 10

;; in octal
#o12 ; ==> 10

;; or any base you want (this is base 3)
#3R102 ; ==> 11

;; binary ratio
#b1010/1011 ; ==> 10/11

;; big number in base 36
#36rABCDEFGHIJKLMNOPQRSTUVWXYZ ; ==> 8337503854730415241050377135811259267835

;; some floating point stuff
123e0 ; ==> 123.0
123e-3 ; ==> 0.123
0.123e20 ; ==> 1.23e+19
123d23 ; ==> 1.23d+25

;; the division operater returns the reciprocal of a number if
;; called with only one argument
(/ 5) ; ==> 1/5

;;;; rounding

;; round down
(floor 5.7) ; ==> 5
(floor -5.7) ; ==> -6

;; round up
(ceiling 5.7) ; ==> 6
(ceiling -5.7) ; ==> -5

;; round towards 0
(truncate 5.7) ; ==> 5
(truncate -5.7) ; ==> -5

;; round to nearest integer
(round 5.7) ; ==> 6
(round 5.2) ; ==> 5
(round -5.7) ; ==> -5
(round -5.2) ; ==> -5


;; Two related functions are MOD and REM, which return the modulus and
;; remainder of a truncating division on real numbers. These two functions
;; are related to the FLOOR and TRUNCATE functions as follows:
(defvar *x* 10)
(defvar *y* 3)
(+ (* (floor    (/ *x* *y*)) *y*) (mod *x* *y*)) ;  === *x*
(+ (* (truncate (/ *x* *y*)) *y*) (rem *x* *y*)) ;  === *x*
;; Thus, for positive quotients they're equivalent, but for negative
;; quotients they produce different results.


;; The functions 1+ and 1- provide a shorthand way to express adding
;; and subtracting one from a number. Note that these are different
;; from the macros INCF and DECF. 1+ and 1- are just functions that
;; return a new value, but INCF  and DECF modify a place. The following
;; equivalences show the relation between INCF/DECF, 1+/1-, and +/-:
;(incf x)    === (setf x (1+ x)) === (setf x (+ x 1))
;(decf x)    === (setf x (1- x)) === (setf x (- x 1))
;(incf x 10) === (setf x (+ x 10))
;(decf x 10) === (setf x (- x 10))


;; comparisons can take more than two values
(= 1 (- 10 9) (*))

;; The /= function, conversely, returns true only if all its arguments
;; are different values.
(/= 1 1) ; ==> NIL
(/= 1 2) ; ==> T
(/= 1 2 3) ; ==> T
(/= 1 2 3 1) ; ==> NIL
(/= 1 2 3 1.0) ; ==> NIL

;; in comparisons each argument is compared to the argument to its right
(< 2 3 4) ; ==> T
(< 2 3 3) ; ==> NIL
(<= 2 3 3) ; ==> T
(<= 2 3 3 4) ; ==> T
(<= 2 3 4 3) ;  ==> NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; garbage collection/memory usage information ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(room)


;;; more number stuff
(max 10 11) ; ==> 11
(min -12 -10) ; ==> -12
(max -1 2 -3) ; ==> 2


;; The P suffix on the names of these functions is a standard naming
;; convention for predicate functions, functions that test some condition and
;; return a boolean.
(zerop 6) ; ==> NIL
(zerop 0) ; ==> T
(minusp 5) ; ==> NIL
(minusp -5) ; ==> T
(plusp -6) ; ==> NIL
(plusp 7) ; ==> T
(evenp 6) ; ==> T
(oddp 6) ; ==> NIL


;; Case-sensitive and case-insensitive character comparison opperators
(char= #\x #\x) ; ==> T
(char= #\x #\X) ; ==> NIL
(char-equal #\x #\X) ; ==> T


;; The rest of the character comparators follow this same naming scheme:
;; the case-sensitive comparators are named by prepending the analogous
;; numeric comparator with CHAR; the case-insensitive versions spell out the
;; comparator name, separated from the CHAR with a hyphen.
;;
; Numeric Analog	Case-Sensitive	Case-Insensitive
;     =	            CHAR=	        CHAR-EQUAL
;     /=	        CHAR/=	        CHAR-NOT-EQUAL
;     <	            CHAR<	        CHAR-LESSP
;     >	            CHAR>	        CHAR-GREATERP
;     <=	        CHAR<=	        CHAR-NOT-GREATERP
;     >=	        CHAR>=	        CHAR-NOT-LESSP


;; String comparisons
;;
; Numeric Analog	Case-Sensitive	Case-Insensitive
;      =	        STRING=	        STRING-EQUAL
;      /=	        STRING/=	    STRING-NOT-EQUAL
;      <	        STRING<	        STRING-LESSP
;      >	        STRING>	        STRING-GREATERP
;      <=	        STRING<=	    STRING-NOT-GREATERP
;      >=	        STRING>=	    STRING-NOT-LESSP
;;
;; However, unlike the character and number comparators, the string
;; comparators can compare only two strings. That's because they also
;; take keyword arguments that allow you to restrict the comparison
;; to a substring of either or both strings.

(string= "foobarbaz" "quuxbarfoo" :start1 3 :end1 6 :start2 4 :end2 7)

;; The comparators that return true when their arguments
;; differ--that is, all of them except STRING= and STRING-EQUAL--return the
;; index in the first string where the mismatch was detected.
(string/= "lisp" "lissome") ; ==> 3
(string/= "lisp" "lisp") ; ==> NIL

;; If the first string is a prefix of the second, the return value
;; will be the length of the first string, that is, one greater
;; than the largest valid index into the string.
(string< "lisp" "lisper") ; ==> 4

;; When comparing substrings, the resulting value is still an index
;; into the string as a whole. For instance, the following compares
;; the substrings "bar" and "baz" but returns 5 because that's the
;; index of the r in the first string:
(string< "foobar" "abaz" :start1 3 :start2 1) ; ==> 5 (N.B. not 2)
