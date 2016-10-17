(define (union-set set1 set2)
  (define (iter rest1 rest2)
	(if (null? rest1)
		rest2
		(iter (cdr rest1) (remove (car rest1) rest2))))
  (append set1 (iter set1 set2)))
