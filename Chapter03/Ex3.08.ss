(define f
  (let ((previous 0))
	(lambda (n)
	  (let ((ret previous))
		(set! previous n)
		ret))))
