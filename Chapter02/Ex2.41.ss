;;A O(n^2) algorithm
(define (triples-of-given-sum n s)
  (define (iter i j res)
	(cond ((and (>= i (min (- n 1) (- s i j 1)))
				(>= j (min n (- s i j)))) res)
		  ((>= j (min n (- s i j)))
		   (iter (+ i 1) (+ i 2) res))
		  (else
		   (iter i (+ j 1) (cons (list i j (- s i j)) res)))))
  (reverse (iter 1 2 '())))
