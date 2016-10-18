(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements);;O(n)
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

(define (tree->list tree);;O(n)
  (define (copy-to-list tree result-list)
	(if (null? tree)
		result-list
		(copy-to-list (left-branch tree)
					  (cons (entry tree)
							(copy-to-list (right-branch tree)
										  result-list)))))
  (copy-to-list tree '()))

(define (union-list set1 set2);;O(n)
  (define (iter s1 s2 res)
	(cond ((null? s1) (append (reverse res) s2))
		  ((null? s2) (append (reverse res) s1))
		  ((= (car s1) (car s2))
		   (iter (cdr s1) (cdr s2) (cons (car s1) res)))
		  ((< (car s1) (car s2))
		   (iter (cdr s1) s2 (cons (car s1) res)))
		  (else
		   (iter s1 (cdr s2) (cons (car s2) res)))))
  (iter set1 set2 '()))

(define (intersection-list set1 set2);;O(n)
  (define (iter s1 s2 res)
	(cond ((null? s1) (reverse res))
		  ((null? s2) (reverse res))
		  ((= (car s1) (car s2))
		   (iter (cdr s1) (cdr s2) (cons (car s1) res)))
		  ((< (car s1) (car s2))
		   (iter (cdr s1) s2 res))
		  (else
		   (iter s1 (cdr s2) res))))
  (iter set1 set2 '()))

;;------------ Something New ------------
(define (union-set set1 set2)
  (list->tree (union-list (tree->list set1)
						  (tree->list set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-list (tree->list set1)
								 (tree->list set2))))
