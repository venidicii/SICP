(define (fib n)
  (define (iter a b i)
	(if (= i 0)
		b
		(iter b (+ a b) (- i 1))))
  (if (or (= 0 n) (= 1 n))
	  1
	  (iter 1 1 n)))
