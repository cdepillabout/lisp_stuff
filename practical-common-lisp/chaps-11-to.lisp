
;; Vectors are Common Lisp's basic integer-indexed collection, and they
;; come in two flavors. Fixed-size vectors are a lot like arrays in a
;; language such as Java: a thin veneer over a chunk of contiguous memory
;; that holds the vector's elements.2 Resizable vectors, on the other
;; hand, are more like arrays in Perl or Ruby, lists in Python, or the
;; ArrayList class in Java: they abstract the actual storage, allowing
;; the vector to grow and shrink as elements are added and removed.

(vector) ; ==> #()
(vector 1) ; ==> #(1)
(vector 1 2) ; ==> #(1 2)

;; MAKE-ARRAY is more general than VECTOR since you can use it to create
;; arrays of any dimensionality as well as both fixed-size and resizable
;; vectors. The one required argument to MAKE-ARRAY is a list containing
;; the dimensions of the array. Since a vector is a one-dimensional
;; array, this list will contain one number, the size of the vector. As a
;; convenience, MAKE-ARRAY will also accept a plain number in the place
;; of a one-item list.
(make-array 5 :initial-element nil) ; ==> #(NIL NIL NIL NIL NIL)

;; MAKE-ARRAY is also the function to use to make a resizable vector.
;; A resizable vector also keeps track of the number of elements actually
;; stored in the vector. This number is stored in the vector's fill
;; pointer.  To make a vector with a fill pointer, you pass MAKE-ARRAY
;; a :fill-pointer argument. For instance, the following call to
;; MAKE-ARRAY makes a vector with room for five elements, but it looks
;; empty because the fill pointer is zero:
(make-array 5 :fill-pointer 0) ; ==> #()

;; To add an element to the end of a resizable vector, you can use the
;; function VECTOR-PUSH. It adds the element at the current value of
;; the fill pointer and then increments the fill pointer by one,
;; returning the index where the new element was added. The function
;; VECTOR-POP returns the most recently pushed item, decrementing the
;; fill pointer in the process.
(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*) ; ==> 0
(vector-push 'b *x*) ; ==> 1
(vector-push 'c *x*) ; ==> 2
;; Now *x* is #(A B C)
(vector-pop *x*) ; ==> C
(vector-pop *x*) ; ==> B
(vector-pop *x*) ; ==> A
;; Now *x* is #()

;; To make an arbitrarily resizable vector, you need to pass
;; MAKE-ARRAY another keyword argument: :adjustable.
(make-array 5 :fill-pointer 0 :adjustable t) ==> #()
;; This call makes an adjustable vector whose underlying memory can be
;; resized as needed. To add elements to an adjustable vector, you use
;; VECTOR-PUSH-EXTEND, which works just like VECTOR-PUSH except it will
;; automatically expand the array if you try to push an element onto a
;; full vector.


;; While frequently used together, the :fill-pointer and :adjustable
;; arguments are independent--you can make an adjustable array without
;; a fill pointer. However, you can use VECTOR-PUSH and VECTOR-POP
;; only with vectors that have a fill pointer and VECTOR-PUSH-EXTEND
;; only with vectors that have a fill pointer and are adjustable. You
;; can also use the function ADJUST-ARRAY to modify adjustable arrays
;; in a variety of ways beyond just extending the length of a vector.


;; Strings are also vectors, so all the functions that can be used with
;; vectors can also be used with strings. But you can't do this:
;; (vector-push #\h "hello") because "hello" doesn't have a fill pointer.


;; You can use MAKE-ARRAY to make resizable strings by adding another
;; keyword argument, :element-type.
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) ; ==>  ""


;; ELT, short for element, takes a sequence and an integer index
;; between zero (inclusive) and the length of the sequence (exclusive)
;; and returns the corresponding element.
(defparameter *xy* (vector 1 2 3))
(length *xy*) ; ==> 3
(elt *xy* 0) ; ==> 1
(elt *xy* 1) ; ==> 2
(elt *xy* 2) ; ==> 3
(elt *xy* 3) ; ==> error
;; ELT is also a SETFable place, so you can set the
;; value of a particular element like this:
(setf (elt *xy* 0) 10) ; now *xy* is #(10 2 3)


;; Additional functions that can be used
(count 1 #(1 2 1 2 3 1 2 3 4)) ; ==> 3
(remove 1 #(1 2 1 2 3 1 2 3 4)) ; ==> #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4)) ; ==> (2 2 3 2 3 4)
(remove #\a "foobarbaz") ; ==> "foobrbz"
(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) ; ==> #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) ; ==> (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz") ; ==> "fooxarxaz"
(find 10 #(10 11 12 10 13 14 15)) ; ==> 10
(find 10 #(1 2 1 2 3 1 2 3 4)) ; ==> NIL
(position 1 #(1 2 1 2 3 1 2 3 4)) ; ==> 0

;; You can modify the behavior of these five functions in a variety
;; of ways using keyword arguments. 
(count "foo" #("foo" "bar" "baz") :test #'string=) ; ==> 1
(count "foo" #("foo" "bar" "baz")) ; ==> 0

;; with the :key keyword you can pass a one-argument function to be
;; called on each element of the sequence to extract a key value,
;; which will then be compared to the item in the place of the element
;; itself.
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first) ; ==> (C 30)

(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first) ; ==> (A 10)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ; ==> (A 30)

(remove #\a "foobarbaz" :count 1) ; ==> "foobrbaz"
(remove #\a "foobarbaz" :count 1 :from-end t) ; ==> "foobarbz"

;; For each of these functions, Common Lisp provides
;; two higher-order function variants.

(count-if #'evenp #(1 2 3 4 5)) ; ==> 2
(count-if-not #'evenp #(1 2 3 4 5)) ; ==> 3
(position-if #'digit-char-p "abcd0001") ; ==> 4
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
			   #("foo" "bar" "baz" "foom")) ; ==> #("foo" "foom")

(remove-if-not #'alpha-char-p
  #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0))) ; ==> #("foo" "bar")

(remove-duplicates #(1 2 1 2 3 1 2 3 4)) ; ==> #(1 2 3 4)

(concatenate 'vector #(1 2 3) '(4 5 6)) ; ==> #(1 2 3 4 5 6)
(concatenate 'list #(1 2 3) '(4 5 6)) ; ==> (1 2 3 4 5 6)
(concatenate 'string "abc" '(#\d #\e #\f)) ; ==> "abcdef"

;; Two sorting functions, #'sort and #'stable-sort.
;; The are equivalent, except that stable-sort guarantees that it
;; will not reorder elements that it deems equivalent.
;; These are both destructive functions, so you should make a
;; copy of the sequence if you want to keep the unsorted sequence.
(sort (vector "foo" "bar" "baz") #'string<) ; ==> #("bar" "baz" "foo")

;; The MERGE function takes two sequences and a predicate and returns
;; a sequence produced by merging the two sequences, according to the
;; predicate. It's related to the two sorting functions in that if
;; each sequence is already sorted by the same predicate, then the
;; sequence returned by MERGE will also be sorted. 
(merge 'vector #(1 3 5) #(2 4 6) #'<) ; ==> #(1 2 3 4 5 6)
(merge 'list #(1 3 5) #(2 4 6) #'<) ; ==> (1 2 3 4 5 6)

