
;; COPY-LIST just does a shallow copy
(defvar *whatwhat* '((1 2 3) (4 5 6)))
(defvar *whowho* (copy-list *whatwhat*))
(setf (caar *whatwhat*) 10)
*whatwhat* ; ==> ((10 2 3) (4 5 6))
*whowho* ; ==> ((10 2 3) (4 5 6))

;; for a deeper copy of the whole list structure, 
;; you can use COPY-TREE
(defvar *wherewhere* '((1 2 3) (4 5 6)))
(defvar *whywhy* (copy-tree *wherewhere*))
(setf (caar *wherewhere*) 10)
*wherewhere* ; ==> ((10 2 3) (4 5 6))
*whywhy* ; ==> ((10 2 3) (4 5 6))


;; functions for comparing trees
(tree-equal '((1 2) (3 4) (5 6)) '((1 2) (3 4) (5 6))) ; ==> T
(tree-equal '((10 20) (3 4) (5 6)) '((1 2) (3 4) (5 6))) ; ==> NIL


;; function for substituting in a tree
(subst 10 1 '(1 2 (3 2 1) ((1 1) (2 2)))) ; ==> (10 2 (3 2 10) ((10 10) (2 2)))


;; how to look up things in a an alist
(assoc 'a '((a . 1) (b . 2) (c . 3))) ; ==> (A . 1)
(assoc 'c '((a . 1) (b . 2) (c . 3))) ; ==> (C . 3)
(assoc 'd '((a . 1) (b . 2) (c . 3))) ; ==> NIL

;; if you wanted to use string keys, you might write this:
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=) ; ==> ("a" . 1)

;; Without specifying :test to be STRING=, that ASSOC would probably
;; return NIL because two strings with the same contents aren't
;; necessarily EQL.
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3))) ; ==> NIL

;; You can add a pair to the front of an alist with CONS  like this:
(cons (cons 'new-key 'new-value) alist)
;; However, as a convenience, Common Lisp provides the function ACONS,
;; which lets you write this:
(acons 'new-key 'new-value alist)
;; Like CONS, ACONS is a function and thus can't modify the place
;; holding the alist it's passed. If you want to modify an alist, you
;; need to write either this:
(setf alist (acons 'new-key 'new-value alist))
;; or this:
(push (cons 'new-key 'new-value) alist)

;; Create an alist from two lists
(pairlis '(a b c) '(1 2 3)) ; ==> ((C . 3) (B . 2) (A . 1))


;; The other kind of lookup table is the property list, or plist.
;; Structurally a plist is just a regular list with the keys and
;; values as alternating values. For instance, a plist mapping A, B,
;; and C, to 1, 2, and 3 is simply the list (A 1 B 2 C 3).

;; However, plists are less flexible than alists. In fact, plists
;; support only one fundamental lookup operation, the function GETF,
;; which takes a plist and a key and returns the associated value or
;; NIL if the key isn't found. Unlike ASSOC, which uses EQL as its
;; default test and allows a different test function to be supplied
;; with a :test argument, GETF always uses EQ to test whether the
;; provided key matches the keys in the plist. Consequently, you
;; should never use numbers or characters as keys in a plist; The
;; behavior of EQ for those types is essentially
;; undefined. Practically speaking, the keys in a plist are almost
;; always symbols, which makes sense since plists were first invented
;; to implement symbolic "properties," arbitrary mappings between
;; names and values.

;; You can use SETF with GETF to set the value associated with a given
;; key. SETF also treats GETF a bit specially in that the first
;; argument to GETF is treated as the place to modify. Thus, you can
;; use SETF of GETF to add a new key/value pair to an existing plist.
(defparameter *plist* ())
*plist* ; ==> NIL
;; this is kind of weird that :a is created
(setf (getf *plist* :a) 1) ; ==> 1
*plist* ; ==> (:A 1)
(setf (getf *plist* :a) 2) ; ==> 2
*plist* ; ==> (:A 2)

;; To remove a key/value pair from a plist, you use the macro REMF,
;; which sets the place given as its first argument to a plist
;; containing all the key/value pairs except the one specified. It
;; returns true if the given key was actually found.
(remf *plist* :a) ; ==> T
*plist* ; ==> NIL
;; Like GETF, REMF always uses EQ to compare the given key to the
;; keys in the plist.


;; Since plists are often used in situations where you want to extract
;; several properties from the same plist, Common Lisp provides a
;; function, GET-PROPERTIES, that makes it more efficient to extract
;; multiple values from a single plist. It takes a plist and a list of
;; keys to search for and returns, as multiple values, the first key
;; found, the corresponding value, and the head of the list starting
;; with the found key. This allows you to process a property list,
;; extracting the desired properties, without continually rescanning
;; from the front of the list.
(defun process-properties (plist keys)
  (loop while plist do
       (multiple-value-bind (key value tail) (get-properties plist keys)
         (when key (process-property key value))
         (setf plist (cddr tail)))))


;; This macro provides a way to destructure arbitrary lists, similar
;; to the way macro parameter lists can take apart their argument
;; list. The basic skeleton of a DESTRUCTURING-BIND is as follows:
;(destructuring-bind (parameter*) list
;  body-form*)
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z)) ; ==> (:X 1 :Y 2 :Z 3)
(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z)) ; ==> (:X 1 :Y (2 20) :Z 3)
(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ; ==> (:X 1 :Y1 2 :Y2 20 :Z 3)
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ; ==> (:X 1 :Y1 2 :Y2 20 :Z 3)
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ; ==> (:X 1 :Y1 2 :Y2 NIL :Z 3)
(destructuring-bind (&key x y z) (list :x 1 :y 2 :z 3)
  (list :x x :y y :z z)) ; ==> (:X 1 :Y 2 :Z 3)
(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z)) ; ==> (:X 3 :Y 2 :Z 1)

;; The &whole parameter, if specified, it must be the first parameter
;; in a parameter list, and it's bound to the whole list form.
(destructuring-bind (&whole whole &key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z :whole whole)) ; ==> (:X 3 :Y 2 :Z 1 :WHOLE (:Z 1 :Y 2 :X 3))
;; When a &whole parameter is used in a macro parameter list, the form
;; it's bound to is the whole macro form, including the name of the
;; macro.
