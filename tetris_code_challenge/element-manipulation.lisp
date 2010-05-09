(in-package :tetris-challenge)

(defun set-element (new-element row column play-space)
  "Set an element of play-space to be new-element at rowXcolumn. This does not mess with play-space, but creates a new play-space"
  (assert (< row (num-rows play-space)))
  (assert (< column (num-columns play-space)))
;  (format t "setting play-space ~ax~a to ~a~%"
;		  row column new-element)
;  (print-play-space play-space)
  (let ((new-play-space (copy-tree play-space)))
	(setf (nth column (nth row new-play-space)) new-element)
	new-play-space))

(defun get-element (row column play-space)
  "Get an element of play-space."
  (assert (< row (num-rows play-space)))
  (assert (< column (num-columns play-space)))
  (nth column (nth row play-space)))

(defmacro loop-over-elements-in-piece
	((piece row column element) &body body)
  "This is a macro to make it easier to loop over every element of a piece and perform some action on that element."
  `(loop
	  for ,row from 0 to (1- (num-rows ,piece))
	  do (loop
			for ,column from 0 to (1- (num-columns ,piece))
			for ,element = (get-element ,row ,column ,piece)
			do (progn ,@body))))

(defun elem-below-this-in-piece (piece row column)
  "Returns true if there is a solid element below the element at ROWxCOLUMN."
  (piece-row-col-assert piece row column)
  (notevery #'(lambda (x) (equal x *empty-symbol*))
			(loop
			   for i from (1+ row) to (1- (length piece))
			   for element = (nth column (nth i piece))
			   collect element)))

(defun elem-below-this-in-table (table piece row column position level)
  "Returns true if there is a solid element in the TABLE below the element in PIECE at ROWxCOLUMN."
  (table-piece-pos-level-assert table piece row column position level)
  (assert (>= (- level (get-row-from-bottom piece row)) 0)
		  () "The element in piece is above the table.")
  (assert (not (elem-below-this-in-piece piece row column))
		  () "There is an element below this one in piece")
  (assert (not (elem-at-bottom-of-table table piece row column position level))
		  () "This element is already at the bottom of table")
  (let ((new-level (- level (get-row-from-bottom piece row))))
	(not (equal (get-element new-level (+ position column) table)
				*empty-symbol*))))

(defun elem-at-bottom-of-table (table piece row column position level)
  "Returns true if the elmeent of piece is at the bottom of the TABLE."
  (table-piece-pos-level-assert table piece row column position level)
  (= (- (1+ level) (get-row-from-bottom piece row))
	 (length table)))

(defun can-drop-element (table piece row column element position level)
  "Returns true if the specific ELEMENT of PIECE (at ROWxCOLUMN) can be dropped
into TABLE at POSITION on LEVEL. LEVEL will always be the level of the lowest
element of piece, not the level of the currently working element."
  (table-piece-pos-level-assert table piece row column position level)
		; if the element is empty, then it can definitely drop
  (cond ((equal element *empty-symbol*) t)
		; if the element is off the table, then it can drop
		((> 0 (- level (get-row-from-bottom piece row))) t)
		; if there is a solid element below this element in piece, then
		; it can drop
		((elem-below-this-in-piece piece row column) t)
		; if this piece is at the bottom of the table, then
		; it cannot drop
		((elem-at-bottom-of-table table piece row column position level)
		 nil)
		; is there anything on the board that would prevernt us from dropping,
		; then we cannot drop.
		((elem-below-this-in-table table piece row column position level)
		 nil)
		; otherwise then we can drop
		(t t)))

