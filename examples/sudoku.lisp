
(in-package :funds-examples)

(defconstant +false+ 0)
(defconstant +true+ 1)

(defun solve (list)
  (list-from-puzzle (puzzle-solve (puzzle-from-list list))))

(defstruct puzzle
  size
  tree)

(defun puzzle-elt (puzzle i j k)
  (tree-find (puzzle-tree puzzle) (index i j k (puzzle-size puzzle))))

(defun elt-true-p (elt)
  (and elt (= elt +true+)))

(defun elt-unknown-p (elt)
  (null elt))

(defun puzzle-from-list (list-rep)
  (labels ((f (puzzle list row i j)
	     (cond ((null list) puzzle)
		   ((null row) (f puzzle (rest list) (first (rest list)) (1+ i) 0))
		   (t (f (if (zerop (first row))
			     puzzle
			     (fill-true puzzle i j (1- (first row))))
			 list (rest row) i (1+ j))))))
    (f (make-puzzle :size (length list-rep)
		    :tree (make-avl-tree))
       list-rep (first list-rep) 0 0)))

(defun list-from-puzzle (puzzle)
  (let ((size (puzzle-size puzzle)))
    (iter (for i below size)
	  (collect (iter (for j below size)
			 (collect (iter (for k below size)
					(when (elt-true-p (puzzle-elt puzzle i j k))
					  (return (1+ k)))
					(finally (return 0)))))))))

(defun puzzle-complete-p (puzzle)
  (= (tree-weight (puzzle-tree puzzle))
     (round (expt (puzzle-size puzzle) 3))))

(defun puzzle-solved-p (puzzle)
  (let ((size (puzzle-size puzzle)))
    (= (round (expt size 2))
       (tree-count +true+ (puzzle-tree
			   puzzle) :key #'bt-value :test #'=))))

(defun fill-true (puzzle i j k)
  (fill-falses (set-to-true puzzle i j k) i j k))

(defun fill-falses (puzzle i j k)
  (let ((b (box-number puzzle i j)))
    (reduce #'(lambda (p group)
		(apply #'fill-falses-group-2 p group))
	    (list (list #'i-j-k-coordinates i j)
		  (list #'j-k-i-coordinates j k)
		  (list #'i-k-j-coordinates i k)
		  (list #'b-k-x-coordinates b k))
	    :initial-value puzzle)))

(defun fill-falses-group (puzzle c-function i j k)
  (let ((size (puzzle-size puzzle)))
    (labels ((f (puzzle x)
	       (if (= x size)
		   puzzle
		   (f (multiple-value-call #'set-to-false 
			puzzle (funcall c-function i j k x size))
		      (1+ x)))))
      (f puzzle 0))))

(defun box-number (puzzle i j)
  (let ((order (order (puzzle-size puzzle))))
    (+ (* order (floor i order))
       (floor j order))))

(defun fill-falses-group-2 (puzzle x-y-z-function x y)
  (let ((size (puzzle-size puzzle)))
    (labels ((f (puzzle z)
	       (if (= z size)
		   puzzle
		   (f (multiple-value-call #'set-to-false 
			puzzle (funcall x-y-z-function x y z size))
		      (1+ z)))))
      (f puzzle 0))))

(defun set-to-true (puzzle i j k)
  (set-value puzzle i j k +true+))

(defun set-to-false (puzzle i j k)
  (set-value puzzle i j k +false+))

(defun set-value (puzzle i j k value)
  (if (puzzle-elt puzzle i j k)
      puzzle
      (let ((size (puzzle-size puzzle)))
	(make-puzzle :size size
		     :tree (tree-insert (puzzle-tree puzzle)
					(index i j k size)
					value)))))
(defun order (size)
  (round (sqrt size)))

(defun index (i j k size)
  (+ (* i size size)
     (* j size)
     k))

(defun debug-print (puzzle)
  (let ((size (puzzle-size puzzle)))
    (iter (for k below size)
	  (format t "~%~%~{~&~{~2A~}~}"
	   (iter (for i below size)
		 (collect (iter (for j below size)
				(collect (or (puzzle-elt puzzle i j k)
					     "")))))))))

(defun puzzle-solve (puzzle)
  (if (puzzle-solved-p puzzle)
      puzzle
      (iter (for f in x-y-z-functions)
	    (for (values x y n) = (best-group puzzle f))
	    (finding (list f x y) minimizing n into (best-list min))
	    (finally (return (apply #'solve-group puzzle best-list))))))

(defun best-group (puzzle x-y-z-function)
  (let ((size (puzzle-size puzzle)))
    (iter (for x below size)
	  (for (values y n) = 
	       (iter (for y below size)
		     (for n = (group-freedom puzzle x-y-z-function x y))
		     (finding y minimizing n into (best-y min))
		     (finally (return (values best-y min)))))
	  (finding x minimizing n into best-x)
	  (finding y minimizing n into (best-y best-n))
	  (finally (return (values best-x best-y best-n))))))

(defun group-freedom (puzzle x-y-z-function x y)
  (let ((size (puzzle-size puzzle)))
    (enlarge-zero
     (iter (for z below size)
	   (counting (elt-unknown-p (multiple-value-call #'puzzle-elt puzzle
							 (funcall x-y-z-function x y z size)))))
     size)))

(defun enlarge-zero (count size)
  (if (zerop count)
      (1+ size)
      count))

(defun solve-group (puzzle x-y-z-function x y)
  (let ((size (puzzle-size puzzle)))  
    (labels ((f (z)
	       (if  (= z size) nil
		    (multiple-value-bind (i j k)
			(funcall x-y-z-function x y z size)
		      (if (puzzle-elt puzzle i j k)
			  (f (1+ z))
			  (or (puzzle-solve (fill-true puzzle i j k))
			      (f (1+ z))))))))
      (f 0))))

(defun i-j-k-coordinates (i j k size)
  (declare (ignore size))
  (values i j k))

(defun i-k-j-coordinates (i k j size)
  (declare (ignore size))
  (values i j k))

(defun j-k-i-coordinates (j k i size)
  (declare (ignore size))
  (values i j k))

(defun b-k-x-coordinates (b k x size)
  (let ((order (order size)))
    (values (+ (* order (floor b order))
	       (floor x order))
	    (+ (* order (mod b order))
	       (mod x order))
	    k)))

(defvar x-y-z-functions (list #'i-j-k-coordinates
			      #'i-k-j-coordinates
			      #'j-k-i-coordinates
			      #'b-k-x-coordinates))

(defun print-sudoku (list)
  (let* ((size (length list))
	 (order (order size)))
    (labels ((f (result list i)
	       (if (null list)
		   result
		   (f (concatenate 'string result
				   (if (zerop (mod i order))
				       (filler-string size)
				       "")
				   (row-as-string (first list)))
		      (rest list)
		      (1+ i)))))
      (concatenate 'string (f (format nil "~%") list 0)
		   (filler-string size)))))

(defun row-as-string (row)
  (let* ((size (length row))
	 (order (order size)))
    (labels ((f (result list i)
	       (if (null list) 
		   result
		   (f (concatenate 'string result 
				 (if (zerop (mod i order)) 
				     "| "
				     "") 
				 (format nil "~2A" (if (zerop (first list))
						       ""
						       (first list))))
		      (rest list)
		      (1+ i)))))
      (concatenate 'string (format nil "~%") (f "" row 0) "|"))))

(defun filler-string (size)
  (let ((order (order size)))
    (labels ((f (result i)
	       (if (= i size)
		   result
		   (f (concatenate 'string result 
				   (if (zerop (mod i order))
				       "+-"
				       "")
				   "--")
		      (1+ i)))))
      (concatenate 'string (format nil "~%") (f "" 0) "+"))))
