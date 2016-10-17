(define (element-of-set? x set)
  (cond ((null? set) #f)
		((equal? (car set) x) #t)
		(else
		 (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (define (iter res1 res2 result)
	(if (null? res1)
		result
		(if (element-of-set? (car res1) res2)
			(iter (cdr res1)
				  (remove (car res1) res2)
				  (cons (car res1) result))
			(iter (cdr res1) res2 result))))
  (iter set1 set2 '()))
