(define (cycle? x)
  (let ((encountered  '()))
	(define (recur x)
	  (cond ((not (pair? x)) false)
			((memq x encountered) true)
			(else
			 (set! encountered (cons x encountered))
			 (recur (cdr x)))))
	(recur x)))
