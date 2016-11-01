(define (make-mointored f)
  (let ((countor 0))
	(lambda (x)
	  (cond ((eq? x 'how-many-calls?) countor)
			((eq? x 'reset-count) (set! countor 0))
			(else
			 (set! countor (+ countor 1))
			 (f x))))))
