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
#+nil
(defun dist-between (u v graph len)
  "Try to find distance either in u or v entry of graph. If LEN is nil
distance defaults to unity."
  (let ((p (position v (elt graph u))))
    (if p
	(if len
	    (elt (elt len u) p)
	    1)
	(let ((p (position u (elt graph v))))
	  (when p
	    (if len
		(elt (elt len v) p)
		1))))))
#+nil
(dist-between 0 2 graph len)

#+nil
(dist-between 2 0 graph len)


(defun dist-between (u v graph len)
  "Find if u and v are connected (by length vertex of length 1)."
  (when (position v (elt graph u))
    1))

#+nil
(dist-between 0 50 graph nil)
#+nil
(dist-between 50 0 graph nil)

(defun dijkstra (graph &key (len nil) (source 0))
  "Return an array of the length of the shortest routes from the
source to the corresponding node."
  (let ((unvisited (loop for i below (length graph) collect i))
	(dist (make-array (length graph) :element-type 'double-float
			 :initial-element double-float-positive-infinity))
	(previous (make-array (length graph) :element-type 'fixnum
			      :initial-element -1)))
    (setf (elt dist source) 0d0)
#+nil    (defparameter *unvisited* unvisited)
#+nil    (defparameter *dist* dist)
    (loop while unvisited do
	 (let ((u (smallest-dist unvisited dist)))
	   (when (= double-float-positive-infinity (aref dist u))
	     (return)) ;; all remaining vertices inaccesible from source
	   (setf unvisited (remove u unvisited))
	   (let ((neighbors (intersection (elt graph u)
					  unvisited)))
	     (format t "~a~%" (list 'neigh neighbors))
	     (dolist (v neighbors)
	       (let ((alt (+ (aref dist u)
			     1
			     #+nil (dist-between u v graph len))))
		 (if (< alt (aref dist v))
		     (setf (aref dist v) alt
			   (aref previous v) u)))))))
    dist))

#+nil
(defparameter *sol* (dijkstra graph :source (+ 49 (* 50 4))))

(+ 49 (* 50 4))

#+nil
(dijkstra graph)

#+nil
(defparameter *sol* (dijkstra graph))

(defun read-pgm (filename)
  (declare ((or pathname string) filename)
	   (values (simple-array (unsigned-byte 8) 2) &optional))
  (with-open-file (s filename)
    (unless (equal (symbol-name (read s)) "P5")
      (error "no PGM file"))
    (let* ((w (read s))
	   (h (read s))
	   (grays (read s))
	   (pos (file-position s))
	   (data (make-array 
		  (list h w)
		  :element-type '(unsigned-byte 8)))
	   (data-1d (make-array 
		     (* h w)
		     :element-type '(unsigned-byte 8)
		     :displaced-to data)))
      (declare ((simple-array (unsigned-byte 8) (* *)) data)
	       ((array (unsigned-byte 8) (*)) data-1d)
	       ((integer 0 65535) grays w h))
      (unless (= grays 255)
	(error "image has wrong bitdepth"))
      (with-open-file (s filename
			 :element-type '(unsigned-byte 8))
	(file-position s pos)
	(read-sequence data-1d s))
      data)))


(defun write-pgm (filename img)
  (declare (simple-string filename)
	   ((array (unsigned-byte 8) 2) img)
	   (values null &optional))
  (destructuring-bind (h w)
      (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
		       :element-type '(unsigned-byte 8)
		       :direction :output
		       :if-exists :append)
      (let ((data-1d (make-array 
		      (* h w)
		      :element-type '(unsigned-byte 8)
		      :displaced-to img)))
	(write-sequence data-1d s)))
    nil))

; a
;lcr
; b
#+nil
(defparameter m (read-pgm "maze.pgm"))
(defparameter aa (make-array (list 50 50)
			    :element-type '(unsigned-byte 8)))
(defparameter bb (make-array (list 50 50)
			    :element-type '(unsigned-byte 8)))
(defparameter ll (make-array (list 50 50)
			    :element-type '(unsigned-byte 8)))
(defparameter rr (make-array (list 50 50)
			    :element-type '(unsigned-byte 8)))
(defun conv (a)
  (if (= a 0)
      0
      1))

(aref m (+ (+ 3 2) -4) (+ (+ 3 2) 8))

(defparameter m2 (make-array (array-dimensions m)
			     :element-type '(unsigned-byte 8)))
(destructuring-bind (h w) (array-dimensions m)
  (dotimes (j h)
   (dotimes (i w)
     (setf (aref m2 j i) (floor (aref m j i) 3)))))
(write-pgm "o.pgm" m2)
#+nil
(dotimes (j 50)
  (dotimes (i 50) ;; centers, border pixels are at 3,4,5
    (let* ((x (+ 3 2 (* 8 i)))
	   (y (+ 3 2 (* 8 j)))
	   (a (conv (aref m (- y 4) x)))
	   (l (conv (aref m y (- x 4))))
	   (r (conv (aref m y (+ x 4))))
	   (b (conv (aref m (+ y 4) x))))
      (setf 
	    (aref m2 (+ y 3) (- x 1)) (* 127 (+ 1 b))
	    (aref m2 (+ y 3) x) (* 127 (+ 1 b))
	    (aref m2 (+ y 3) (+ x 1)) (* 127 (+ 1 b))
	  
	    (aref m2 (- y 3) (- x 1)) (* 127 (+ 1 a))
	    (aref m2 (- y 3) x) (* 127 (+ 1 a))
	    (aref m2 (- y 3) (+ x 1)) (* 127 (+ 1 a))
	    
	    (aref m2 (- y 1) (- x 3)) (* 127 (+ 1 l))
	    (aref m2 y (- x 3)) (* 127 (+ 1 l))
	    (aref m2 (+ y 1) (- x 3)) (* 127 (+ 1 l))
	    
	    (aref m2 (- y 1) (+ x 3)) (* 127 (+ 1 r))
	    (aref m2 y (+ x 3)) (* 127 (+ 1 r))
	    (aref m2 (+ y 1) (+ x 3)) (* 127 (+ 1 r))

	    (aref m2 y x) 255
)
      (setf (aref aa j i) a
	    (aref bb j i) b
	    (aref ll j i) l
	    (aref rr j i) r))))
(defparameter graph (make-array (* 50 50) :element-type 'cons
				:initial-element nil))


#+nil
(dotimes (j 50)
  (dotimes (i 50)
    (let ((p (array-row-major-index aa j i)))
      (when (and (< j 48) (= 1 (aref bb j i)))
	(push (array-row-major-index aa (+ j 1) i)
	      (aref graph p)))
      (when (and (< 0 j) (= 1 (aref aa j i)))
	(push (array-row-major-index aa (- j 1) i)
	      (aref graph p)))
      (when (and (< i 48) (= 1 (aref rr j i)))
	(push (array-row-major-index aa j (+ i 1))
	      (aref graph p)))
      (when (and (< 0 i) (= 1 (aref ll j i)))
	(push (array-row-major-index aa j (- i 1))
	      (aref graph p))))))  
(aref bb 0 1)


(aref aa 0 2)

#+nil
(defparameter *sol* (dijkstra graph :source (array-row-major-index aa 9 49)))

(defparameter o (make-array (list 50 50)
			    :element-type '(unsigned-byte 8)))
#+nil
(dotimes (j 50)
  (dotimes (i 50)
    (let ((p (+ (* j 50) i)))
      (setf (aref o j i) (floor (if (< (elt *sol* p) 255)
				    (elt *sol* p)
				    255))))))
#+nil
(write-pgm "o.pgm" o)
