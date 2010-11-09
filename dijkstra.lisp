;; It is a directed graph because I didn't want to write so much.
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
#+nil
(defun smallest-dist (unvisited dist)
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

(defun smallest-dist (unvisited dist)
  "Return vertex with smallest distance."
  (car 
   (cdr 
    (reduce #'(lambda (a b)
		(if (< (car a) (car b))
		    a
		    b))
	    (map 'cons
		 #'(lambda (i) (list (aref dist i) i)) 
		 unvisited)))))

#+nil
(smallest-dist *unvisited* *dist*)


;; potentially one could leave out one case if the input graph isn't
;; directed
(defun dist-between (u v graph len)
  "Try to find distance either in u or v entry of graph."
  (let ((p (position v (elt graph u))))
    (if p
	(elt (elt len u) p)
	(let ((p (position u (elt graph v))))
	  (when p
	    (elt (elt len v) p))))))
#+nil
(dist-between 0 2 graph len)

#+nil
(dist-between 2 0 graph len)

(defun dijkstra (graph len &optional (source 0))
  "Return an array of the length of the shortest routes from the
source to the corresponding node."
  (let ((unvisited (loop for i below (length graph) collect i))
	(dist (make-array (length graph) :element-type 'double-float
			 :initial-element double-float-positive-infinity))
	(previous (make-array (length graph) :element-type 'fixnum
			      :initial-element -1)))
    (setf (elt dist source) 0d0)
    (defparameter *unvisited* unvisited)
    (defparameter *dist* dist)
    (loop while unvisited do
	 (let ((u (smallest-dist unvisited dist)))
	   (when (= double-float-positive-infinity (aref dist u))
	     (return)) ;; all remaining vertices inaccesible from source
	   (setf unvisited (remove u unvisited))
	   (let ((neighbors (intersection (elt graph u)
					  unvisited)))
	     (dolist (v neighbors)
	      (let ((alt (+ (aref dist u)
			    (dist-between u v graph len))))
		(if (< alt (aref dist v))
		    (setf (aref dist v) alt
			  (aref previous v) u)))))))
    dist))

#+nil
(dijkstra graph len 0)