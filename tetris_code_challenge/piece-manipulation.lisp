(in-package :tetris-challenge)

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


(defun print-piece (piece)
  "Print a piece.  Right now this just calls print-play-table."
  (print-play-space piece))

(defun create-play-space
	(&optional (rows *num-of-rows*) (columns *num-of-columns*))
  "Create the play table or a space for a new blank piece with the 
desired number of rows and columns."
  (loop for i upto (1- rows)
	 collect (make-list columns :initial-element *empty-symbol*)))

(defun print-play-space (play-space)
  "Just print the play table.  This also works to print any arbitrary piece."
  (format t "~{~{~a ~}~%~}~%" play-space))



(defun rotate-piece (piece)
  "Rotate a piece clockwise and produce a new piece. This does not mess with piece, but creates a new piece."
  (let* ((old-rows (num-rows piece))
		 (old-cols (num-columns piece))
		 (new-piece (create-play-space old-cols old-rows)))
	(loop for i from 0 to (1- old-rows)
	   do (loop for j from 0 to (1- old-cols)
			 do (progn
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

(defun get-row-from-bottom (piece row)
  "Get the row from the bottom of the piece.  For instance, passing row 3 of an i-piece will return 0, since it is the bottom row of the i-piece."
  (piece-row-col-assert piece row)
  (- (1- (length piece)) row))

(defun can-drop-piece-at-level-helper (table piece position level)
  (loop
	 for row from 0 to (1- (length piece))
	 collect (loop
				for column from 0 to (1- (length (nth row piece)))
				for element = (nth column (nth row piece))
				collect (can-drop-element table piece row
										  column element position level))))

(defun can-drop-piece-at-level (table piece position level)
  "Returns true if we can drop this piece into level and position of table."
  (every #'(lambda (x) (eq x t))
		 (flatten (can-drop-piece-at-level-helper table piece position level))))


(defun can-drop-piece-at-pos (table piece pos)
  "Returns true if we can drop piece at pos and it will fit in the table."
  (if (< (length table) (length piece))
	  nil
	  (every #'(lambda (x) (eq x t))
			 (loop for level from 0 to (1- (length piece))
				collect (can-drop-piece-at-level table piece pos level)))))

(defun put-piece-at-level (table piece pos level)
  "Return a TABLE with new piece PIECE at level LEVEL."
  ;(assert (can-put-piece-at-level table piece position level))
  (let ((new-table (copy-tree table)))
	(loop for row from 0 to (1- (length piece))
	   do (loop for column from 0 to (1- (length (car piece)))
			 do (let ((piece-element (get-element row column piece))
					  (new-level (- level (get-row-from-bottom piece row)))
					  (new-pos (+ pos column)))
				  (format t "setting piece-element: ~a, new-level: ~a, new-pos: ~a~%"
						  piece-element new-level new-pos)
				  (setf new-table (set-element piece-element new-level new-pos new-table)))))
	new-table))

(defun drop-piece-in-table (table piece pos)
  "Returns a table with piece dropped in it at pos."
  (assert (can-drop-piece-at-pos table piece pos) (table piece pos)
		  "Cannot drop PIECE(~a) at POS(~a) in TABLE(~a)"
		  piece pos table)
  (let ((new-level 
		 (loop
			for i from 0 to (1- (length table))
			while (can-drop-piece-at-level table piece pos i)
			maximize i)))
	(put-piece-at-level table piece pos new-level)))


;; TODO: make sure pieces drop all the way down to the lowest level
;; make sure pieces don't erase parts of other pieces when they are dropped.
;;   (for instance, if you drop a j-piece upside down on another j-piece,
;;    part of the first one will get erased)
