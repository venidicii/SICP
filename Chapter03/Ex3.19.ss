(define (cycle? x)
  (define (recur x y)
	(cond ((not (and (pair? x) (pair? y)
					 (pair? (cdr x)) (pair? (cdr y))
					 (pair? (cddr y)))) false)
		  ((eq? x y) true)
		  (else
		   (recur (cdr x) (cddr y)))))
  (if (or (not (pair? x))
		  (not (pair? (cdr x)))
		  (not (pair? (cddr x))))
	  false
	  (recur (cdr x) (cddr x))))
