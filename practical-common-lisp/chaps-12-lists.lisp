
;; Both CAR and CDR are also SETFable places--given an existing cons
;; cell, it's possible to assign a new value to either of its values.

(defparameter *cons* (cons 1 2))
*cons* ; ==> (1 . 2)
(setf (car *cons*) 10) ; ==> 10
*cons* ; ==> (10 . 2)
(setf (cdr *cons*) 20) ; ==> 20
*cons* ; ==> (10 . 20)

; (cons 1 nil) == (list 1)
; (first *list*) == (car *list*)
; (rest *list*) == (cdr *list*)

(defparameter *what* (cons 1 '()))
*what* ; ==> (1)
(setf (cdr *what*) (list 2 3 4))
*what* ; ==> (1 2 3 4)



(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))
;; After evaluating these forms, you have three lists, but *list-3*
;; and *list-2* share structure just like the lists in the previous
;; diagram.
*list-1* ; ==> (1 2)
*list-2* ; ==> (3 4)
*list-3* ; ==> (1 2 3 4)
;; Now consider what happens when you modify *list-2*.
(setf (first *list-2*) 0) ; ==> 0
*list-2* ; ==> (0 4)     ; as expected
*list-3* ; ==> (1 2 0 4) ; maybe not what you wanted
;; The change to *list-2* also changes *list-3* because of the shared
;; structure: the first cons cell in *list-2* is also the third cons
;; cell in *list-3*.

;; let's compare REVERSE, the nondestructive function that returns a
;; reversed version of a sequence, to NREVERSE, a recycling version of
;; the same function. Because REVERSE doesn't modify its argument, it
;; must allocate a new cons cell for each element in the list being
;; reversed. But suppose you write something like this:
;  (setf *list* (reverse *list*))
;; By assigning the result of REVERSE back to *list*, you've removed
;; the reference to the original value of *list*. Assuming the cons
;; cells in the original list aren't referenced anywhere else, they're
;; now eligible to be garbage collected. However, in many Lisp
;; implementations it'd be more efficient to immediately reuse the
;; existing cons cells rather than allocating new ones and letting the
;; old ones become garbage. NREVERSE allows you to do exactly
;; that. The N stands for non-consing, meaning it doesn't need to
;; allocate any new cons cells.

;; In general, the recycling functions have names that are the same as
;; their non-destructive counterparts except with a leading
;; N. However, not all do, including several of the more commonly used
;; recycling functions such as NCONC, the recycling version of APPEND,
;; and DELETE, DELETE-IF, DELETE-IF-NOT, and DELETE-DUPLICATES, the
;; recycling versions of the REMOVE family of sequence functions.


;; Like APPEND, NCONC returns a concatenation of its list arguments,
;; but it builds its result in the following way: for each nonempty
;; list it's passed, NCONC sets the CDR of the list's last cons cell
;; to point to the first cons cell of the next nonempty list. It then
;; returns the first list, which is now the head of the
;; spliced-together result. Thus:
(defparameter *x* (list 1 2 3))
(nconc *x* (list 4 5 6)) ; ==> (1 2 3 4 5 6)
*x* ; ==> (1 2 3 4 5 6)



;; In practice, recycling functions tend to be used in a few idiomatic
;; ways. By far the most common recycling idiom is to build up a list
;; to be returned from a function by "consing" onto the front of a
;; list, usually by PUSHing elements onto a list stored in a local
;; variable and then returning the result of NREVERSEing it.
(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(upto 10) ; ==> (0 1 2 3 4 5 6 7 8 9)




;; The next most common recycling idiom9 is to immediately reassign
;; the value returned by the recycling function back to the place
;; containing the potentially recycled value. (delete is recycling
;; version of remove)
;(setf foo (delete nil foo))

;; This sets the value of foo to its old value except with all the
;; NILs removed. However, even this idiom must be used with some
;; care--if foo shares structure with lists referenced elsewhere,
;; using DELETE instead of REMOVE can destroy the structure of those
;; other lists. (these are *list-2 and *list-3* from earlier)
*list-2* ; ==> (0 4)
*list-3* ; ==> (1 2 0 4)
(setf *list-3* (delete 4 *list-3*)) ; ==> (1 2 0)
;; However, DELETE will likely perform the necessary deletion by
;; setting the CDR of the third cons cell to NIL, disconnecting the
;; fourth cons cell, the one holding the 4, from the list. Because the
;; third cons cell of *list-3* is also the first cons cell in
;; *list-2*, the following modifies *list-2* as well:
*list-2* ; ==> (0)
;; If you had used REMOVE instead of DELETE, it would've built a list
;; containing the values 1, 2, and 0, creating new cons cells as
;; necessary rather than modifying any of the cons cells in
;; *list-3*. In that case, *list-2* wouldn't have been affected.


;; One last gotcha to watch out for is that the sorting functions
;; SORT, STABLE-SORT, and MERGE mentioned in Chapter 11 are also
;; recycling functions when applied to lists.10 However, these
;; functions don't have nondestructive counterparts, so if you need to
;; sort a list without destroying it, you need to pass the sorting
;; function a copy made with COPY-LIST.
(defparameter *list* (list 4 3 2 1))
(sort *list* #'<) ; ==> (1 2 3 4)
*list* ; ==> (4)          


;; Common Lisp provides functions named for the other ordinals from
;; SECOND to TENTH that return the appropriate element. More
;; generally, the function NTH takes two arguments, an index and a
;; list, and returns the nth (zero-based) element of the
;; list. Similarly, NTHCDR takes an index and a list and returns the
;; result of calling CDR n times. (Thus, (nthcdr 0 ...) simply returns
;; the original list, and (nthcdr 1 ...) is equivalent to REST.) Note,
;; however, that none of these functions is any more efficient, in
;; terms of work done by the computer, than the equivalent
;; combinations of FIRSTs and RESTs--there's no way to get to the nth
;; element of a list without following n CDR references.

;; NTH is roughly equivalent to the sequence function ELT but works
;; only with lists. Another difference is that ELT will signal an
;; error if you try to access an element at an index greater than or
;; equal to the length of the list, but NTH will return NIL.


;;; Other functions

;; LAST	      Returns the last cons cell in a list. With an integer, argument
;;            returns the last n cons cells.
;; BUTLAST	  Returns a copy of the list, excluding the last cons cell. With
;;            an integer argument, excludes the last n cells.
;; NBUTLAST	  The recycling version of BUTLAST; may modify and return the
;;            argument list but has no reliable side effects.
;; LDIFF	  Returns a copy of a list up to a given cons cell.
;; TAILP	  Returns true if a given object is a cons cell that's part of
;;            the structure of a list.
;; LIST*	  Builds a list to hold all but the last of its arguments and then
;;            makes the last argument the CDR of the last cell in the list. In
;;            other words, a cross between LIST and APPEND.
;; MAKE-LIST  Builds an n item list. The initial elements of the list are NIL
;;            or the value specified with the :initial-element keyword argument.
;; REVAPPEND  Combination of REVERSE and APPEND; reverses first argument as with
;;            REVERSE and then appends the second argument.
;; NRECONC	  Recycling version of REVAPPEND; reverses first argument as if by
;;            NREVERSE and then appends the second argument. No reliable side
;;            effects.
;; CONSP	  Predicate to test whether an object is a cons cell.
;; ATOM	      Predicate to test whether an object is not a cons cell.
;; LISTP	  Predicate to test whether an object is either a cons cell or NIL.
;; NULL	      Predicate to test whether an object is NIL. Functionally equivalent
;;            to NOT but stylistically preferable when testing for an empty list
;;            as opposed to boolean false.



;; MAPCAR is the function most like MAP. Because it always returns a
;; list, it doesn't require the result-type argument MAP does.
(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3)) ; ==> (2 4 6)
(mapcar #'+ (list 1 2 3) (list 10 20 30)) ; ==> (11 22 33)

;; MAPLIST is just like MAPCAR except instead of passing the elements
;; of the list to the function, it passes the actual cons cells.
;; Thus, the function has access not only to the value of each element
;; of the list (via the CAR of the cons cell) but also to the rest of
;; the list (via the CDR).

;; MAPCAN and MAPCON work like MAPCAR and MAPLIST except for the way
;; they build up their result. While MAPCAR and MAPLIST build a
;; completely new list to hold the results of the function calls,
;; MAPCAN and MAPCON build their result by splicing together the
;; results--which must be lists--as if by NCONC. Thus, each function
;; invocation can provide any number of elements to be included in the
;; result. MAPCAN, like MAPCAR, passes the elements of the list to
;; the mapped function while MAPCON, like MAPLIST, passes the cons
;; cells.
