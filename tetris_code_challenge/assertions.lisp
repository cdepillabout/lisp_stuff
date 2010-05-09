(in-package :tetris-challenge)

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
