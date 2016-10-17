(define (fold-right op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
	(if (null? rest)
		result
		(iter (op result (car rest))
			  (cdr rest))))
  (iter initial sequence))

(define op1 +)
(define op2 *)

(fold-right op2 1 (list 1 2 3))
(fold-left op2 1 (list 1 2 3))
