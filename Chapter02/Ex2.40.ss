(require math/number-theory)

(define (range a b)
  (if (> a b)
	  '()
	  (cons a (range (+ a 1) b))))

(define (unique-pairs n)
  (apply append
		 (map (lambda (x)
				(map (lambda (y)
					   (cons x (list y)))
					 (range 1 x)))
			  (range 2 n))))

(define (prime-sum-pairs n)
  (define (filter seq)
	(cond ((null? seq) '())
		  ((prime? (apply + (car seq)))
		   (cons (append (car seq) (list (apply + (car seq))))
				 (filter (cdr seq))))
		  (else (filter (cdr seq)))))
  (filter (unique-pairs n)))
