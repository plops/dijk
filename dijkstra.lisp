(defparameter graph
  '((1 2 5)				; 0
    (3 2)				; 1
    (3 5)				; 2
    (4)					; 3
    (5)					; 4
    ()))				; 5

(defparameter len
  '((7 9 14)				; 0
    (15 10)				; 1
    (11 2)				; 2
    (6)					; 3
    (9)					; 4
    ()))


(defparameter dist 
  '(nil nil nil nil nil nil))

(defparameter prev-dist 
  '(nil nil nil nil nil nil))

(defparameter unvisited
  '(0 1 2 3 4 5))

(defun smallest-dist (ls)
  (let* ((min-idx (car ls))
	 (min-val (elt dist min-idx)))
    (dolist (e (cdr ls))
      (let ((val (elt dist e)))
	(when (and val (< val min-val))
	  (setf min-val val
		min-idx e))))
    min-idx))
#+nil
(smallest-dist '(0 1 2 3 4 5))
#+nil
(smallest-dist '(1 0 2 3 4 5))

(let ((cur 0))
  (setf (elt dist cur) 0
	(elt prev-dist cur) 0)
  (loop while unvisited do
       (let ((u (smallest-dist unvisited)))
	 )
       (let ((neighbors (elt graph cur))))))
