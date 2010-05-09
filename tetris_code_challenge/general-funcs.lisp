(in-package :tetris-challenge)

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
