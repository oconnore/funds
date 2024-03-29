
(in-package :funds)

(defstruct dict 
  hash
  test
  tree)

(defun make-dictionary (&key (hash #'sxhash) (test #'eql))
  "An empty dictionary that hashes occording to the given hash function,
which defaults to #'sxhash and and tests according to the given test 
function, which defaults to #'eql."
  (make-dict :hash hash :test test :tree (make-avl-tree)))

(defun dictionary-add (d k v)
  "A dictionary similar to the given dictionary except that k maps to
v in the returned dictionary."
  (let* ((h (funcall (dict-hash d) k))
	 (old-alist (tree-find (dict-tree d) h))
	 (new-alist (acons k v (remove (assoc k old-alist :test (dict-test d))
				       old-alist))))
    (make-dict :hash (dict-hash d)
	       :test (dict-test d)
	       :tree (tree-insert (dict-tree d) h new-alist))))

(defun dictionary-remove (d k)
  "A dictionary similar to the given dictionary, except that k does
not map to any value in the returned dictionary."
  (let* ((h (funcall (dict-hash d) k))
	 (old-alist (tree-find (dict-tree d) h))
	 (new-alist (remove (assoc k old-alist :test (dict-test d))
			    old-alist)))
    (make-dict :hash (dict-hash d)
	       :test (dict-test d)
	       :tree (if (null new-alist)
			 (tree-remove (dict-tree d) h)
			 (tree-insert (dict-tree d) h new-alist)))))

(defun dictionary-lookup (d k)
  "The value associated with the given key in the given dictionary.  A second
value is returned to indicate whether the key is associated with any value or
is not found."
  (let ((pair (assoc k 
		     (tree-find (dict-tree d) (funcall (dict-hash d) k))
		     :test (dict-test d))))
    (if (null pair)
	(values nil nil)
	(values (cdr pair) t))))

(defun dictionary-as-alist (d)
  "An alist containing the same key-value pairs as the given dictionary."
  (labels ((f (tree)
	     (if (tree-empty-p tree)
		 nil
		 (append (f (bt-left tree))
			 (bt-value tree)
			 (f (bt-right tree))))))
    (f (dict-tree d))))

(defun dictionary-from-alist (alist &key (test #'eql) (hash #'sxhash))
  (reduce #'(lambda (d pair)
	      (dictionary-add d (car pair) (cdr pair)))
	  alist
	  :initial-value (make-dictionary :test test :hash hash)))