
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

(defun can-drop-element (table piece row column element position level)
  "Returns true if the specific ELEMENT of PIECE (at ROWxCOLUMN) can be dropped
into TABLE at POSITION on LEVEL."
  (cond ((equal element *empty-symbol*) t)

		;; TODO, maybe I should have something that gets the row from
		;; the bottom of the piece.  that would help....

		;; if we are off the board, then we can drop it
		
	    ;; check if there is an element below this element 
		;; within piece. If so, then we are safe to drop it.

		;; if there is no element below this element, then check
		;; if there is anything on the board that would prevent us
		;; from dropping it.
		
  nil)

  
;; TODO: This isn't exactly what we want.  We want to be able to place
;; a piece stategically in the table, not nesecarily just drop it into
;; place from above.  But it looks like the game does not take this 
;; into account.
(defun drop-piece-in-table (tabel piece position)
    table)
