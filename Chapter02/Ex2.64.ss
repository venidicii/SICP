(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
	  (cons '() elts)
	  (let* ((left-size (quotient (- n 1) 2));;1
			 (left-result (partial-tree elts left-size));;n/2
			 (left-tree (car left-result));;1
			 (non-left-elts (cdr left-result));;1
			 (right-size (- n left-size 1));;1
			 (this-entry (car non-left-elts));;1
			 (right-result (partial-tree (cdr non-left-elts)
										 right-size));;n/2
			 (right-tree (car right-result));;1
			 (remaining-elts (cdr right-result)));;1
		(cons (make-tree this-entry left-tree right-tree);;1
			  remaining-elts))))

;;T(n) = T(n/2)/2+8 => T(n) = (8+c)*n-8
;;The order of growth is O(n)
