(in-package :symbols-and-pieces)

(defparameter *empty-symbol* ".")
(defparameter *i-symbol* "i")
(defparameter *j-symbol* "j")
(defparameter *l-symbol* "l")
(defparameter *o-symbol* "o")
(defparameter *s-symbol* "s")
(defparameter *t-symbol* "t")
(defparameter *z-symbol* "z")

;; Create the pieces we will be using.
(create-piece *i-piece*
  "i"
  "i"
  "i"
  "i")
(create-piece *j-piece*
  ". j"
  ". j"
  "j j")
(create-piece *l-piece*
  "l ."
  "l ."
  "l l")
(create-piece *o-piece*
  "o o"
  "o o")
(create-piece *s-piece*
  ". s s"
  "s s .")
(create-piece *t-piece*
  "t t t"
  ". t .")
(create-piece *z-piece*
  "z z ."
  ". z z")
