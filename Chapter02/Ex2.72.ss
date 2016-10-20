(define (encode message tree)
  (if (null? message)
	  '()
	  (append (encode-symbol (car message) tree)
			  (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (if (memq sym (symbols tree))
	  (if (memq sym (symbols (left-branch tree)));;n/2
		  (if (leaf? (left-branch tree));;1
			  (list 0);;1
			  (cons 0 (encode-symbol sym (left-branch tree))));;T(n/2)
		  (if (leaf? (right-branch tree));;1
			  (list 1);;1
			  (cons 1 (encode-symbol sym (right-branch tree)))));;T(n/2)
	  (error "bad symbol -- ENCODE-SYMBOL?" sym)))

;;Solve the Recursive Equation:
;;T(n) = T(n/2) +n/2 + 2, we get
;;T(n) = C1*Log(n) + n + C2
;;Which means T(n) ~ O(Log(n)).
