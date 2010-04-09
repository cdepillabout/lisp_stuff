
(defparameter *empty-symbol* ".")
(defparameter *i-symbol* "i")
(defparameter *j-symbol* "j")
(defparameter *l-symbol* "l")
(defparameter *o-symbol* "o")
(defparameter *s-symbol* "s")
(defparameter *t-symbol* "t")
(defparameter *z-symbol* "z")

(defun create-piece-helper (&rest rest)
  "Parse the piece string(s) we pass in and create lists representing the piece."
  (loop for row in rest
	 collect (loop for char across row
				when (not (equal " " (string char)))
				collect (string char))))

(defmacro create-piece (piece-name &body body)
  "This just makes it easier to create pieces."
  `(defparameter ,piece-name
	 (create-piece-helper ,@body)))

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
					
(defun print-piece (piece)
  "Print a piece.  Right now this just calls print-play-table."
  (print-play-table piece))
						 
(defparameter *num-of-columns* 10)
(defparameter *num-of-rows* 20)

(defun create-play-space
	(&optional (rows *num-of-rows*) (columns *num-of-columns*))
  "Create the play table or a space for a new blank piece with the 
desired number of rows and columns."
  (loop for i upto (1- rows)
	 collect (make-list columns :initial-element *empty-symbol*)))

(defun print-play-space (play-space)
  "Just print the play table.  This also works to print any arbitrary piece."
  (format t "~{~{~a ~}~%~}~%" play-space))

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

(defun num-rows (piece)
  "Compute the number of rows of a piece takes up."
  (length piece))

(defun num-columns (piece)
  "Compute the number of columns a piece takes up. Make sure all of the
rows have the same number of characters."
  (let ((len (length (first piece))))
	(loop for row in piece
		 do (assert (= len (length row))))
	len))

(defun rotate-piece (piece)
  "Rotate a piece clockwise and produce a new piece. This does not mess with piece, but creates a new piece."
  (let* ((old-rows (num-rows piece))
		 (old-cols (num-columns piece))
		 (new-piece (create-play-space old-cols old-rows)))
;	(format t "old rows: ~a, old-cols: ~a, old-piece:~%"
;			old-rows old-cols)
;	(print-play-space piece)
;	(format t "new piece:~%")
;	(print-play-space new-piece)
	(loop for i from 0 to (1- old-rows)
	   do (loop for j from 0 to (1- old-cols)
			 do (progn
				  ;(format t "i: ~a, j: ~a~%" i j)
				  (setf new-piece
						(set-element (get-element i j piece)
									 j i new-piece)))))
	(flip-piece new-piece)))

(defun flip-piece (piece)
  "Reverse all the rows in piece, there by creating a mirror image of piece. This does not modify piece, but creates a new piece."
  (loop for row in piece
	 collect (reverse row)))
  
(defun rotate-90 (piece)
  "Rotate a piece 90 degress clockwise."
  (rotate-piece piece))

(defun rotate-180 (piece)
  "Rotate a piece 180 degrees."
  (rotate-90 (rotate-90 piece)))

(defun rotate-270 (piece)
  "Rotate a piece 270 degrees clockwise."
  (rotate-90 (rotate-180 piece)))

(defmacro loop-over-elements-in-piece
	((piece row column element) &body body)
  "This is a macro to make it easier to loop over every element of a piece and perform some action on that element."
  `(loop
	  for ,row from 0 to (1- (num-rows ,piece))
	  do (loop
			for ,column from 0 to (1- (num-columns ,piece))
			for ,element = (get-element ,row ,column ,piece)
			do (progn ,@body))))


(defun can-drop (table piece position level)
  "Return true if PIECE can move into LEVEL of TABLE at POSITION.
POSITION 0 is the leftmost edge of TABLE, and LEVEL 0 is the very top
row of table."
  (if (< (num-columns table) (+ position (num-columns piece)))
	  nil
	  (block can-drop-elements-block
		(loop-over-elements-in-piece (piece row column element)
		   (unless (can-drop-element
					table piece row column element position level)
			 (return-from can-drop-elements-block nil)))
		t)))

(defun piece-row-col-assert (piece row &optional column)
  "This just makes sure we aren't passing in any values that we should not be passing in."
  (assert (< row (length piece)) (row piece) "ROW(~a) is outside of PIECE(~a)."
		  row piece)
  (assert (>= row 0) (row) "ROW(~a) is too small." row)
  (when (not (null column))
	  (progn
		(assert (>= column 0) (column) "COLUMN(~a) is too small." column)
		(assert (< column (length (nth row piece)))
				(column) "COLUMN(~a) is too long." column))))

(defun table-piece-pos-level-assert (table piece row column pos level)
  "Make sure everything will fit.  Level is not out of bounds, pos is not out of bounds, and adding piece to the table will not exceed the width of the table."
  (piece-row-col-assert piece row column)
  (assert (>= level 0) (level)
		  "LEVEL(~a) is above the table. (So LEVEL is too low)" level)
  (assert (< level (length table)) (level)
		  "LEVEL(~a) is below the table. (So LEVEL is too high)" level)
  (assert (>= pos 0) (pos) "POS(~a) is too little.  Below 0" pos)
  (assert (< pos (length (car table)))
		  (pos) "POS(~a) is off the right of TABLE." pos)
  (assert (<= (+ pos (length (car piece))) (length (car table)))
		  (pos) "The length of PIECE plus POSITION(~a) is too wide for TABLE."
		  pos)
  (assert (<= (- (1+ level) (get-row-from-bottom piece row))
				 (length table))
			  (level row table)
			  "Final row(~a) position(~a) is below the bottom of length TABLE(~a)"
			  row (- (1+ level) (get-row-from-bottom piece row)) (1- (length table))))
		  

(defun get-row-from-bottom (piece row)
  "Get the row from the bottom of the piece.  For instance, passing row 3 of an i-piece will return 0, since it is the bottom row of the i-piece."
  (piece-row-col-assert piece row)
  (- (1- (length piece)) row))

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
  "Returns true if the element of piece is at the bottom of the TABLE."
  (table-piece-pos-level-assert table piece row column position level)
  ;; TODO, this is where my error is!!
  (= (+ (1+ level) row)
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


(defun flatten (list)
  "Flattens a tree into just a flat list of it's atoms."
  (if (null list)
	  nil
	  (let ((first-elem (first list))
			(remaining (rest list)))
		(cond ((atom first-elem)
			   (cons first-elem (flatten remaining)))
			  ((listp first-elem)
			   (append (flatten first-elem) (flatten remaining)))))))

(defun can-drop-piece-helper (table piece position level)
  (loop
	 for row from 0 to (1- (length piece))
	 collect (loop
				for column from 0 to (1- (length (nth row piece)))
				for element = (nth column (nth row piece))
				collect (can-drop-element table piece row
										  column element position level))))

(defun can-drop-piece (table piece position level)
  "Returns true if we can drop this piece into level and position of table."
  ;(every #'(lambda (x) (eq x t))
		 (flatten (can-drop-piece-helper table piece position level)))
  
;; TODO: This isn't exactly what we want.  We want to be able to place
;; a piece stategically in the table, not nesecarily just drop it into
;; place from above.  But it looks like the game does not take this 
;; into account.
;(defun drop-piece-in-table (table piece position &optional (level 0))
;  (if (can-drop-element  
;    table)

;(defun 