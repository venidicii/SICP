;;------------ Some thing unchanged ------------
(define (deriv exp var)
  (cond ((number? exp) 0)
		((variable? exp)
		 (if (same-variable? exp var) 1 0))
		((sum? exp)
		 (make-sum (deriv (addend exp) var)
				   (deriv (augend exp) var)))
		((product? exp)
		 (make-sum
		  (make-product (multiplier exp)
						(deriv (multiplicand exp) var))
		  (make-product (deriv (multiplier exp) var)
						(multiplicand exp))))
		((expontiation? exp)
		 (let ((base-mid (deriv (base exp) var)))
		   (if (=number? base-mid 0)
			   0
			   (make-product (exponent exp)
							 (make-expontiation (base exp) (- (exponent exp) 1))))))
		(else
		 (error "unknown expression type -- DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (product? exp)
  (and (pair? exp) (eq? (cadr exp) '*)))

(define (multiplier exp)
  (car exp))
  
(define (multiplicand exp)
  (let ((rest (cddr exp)))
	(if (= 1 (length rest))
		(car rest)
		rest)))

;;------------ Some thing new ------------

(define (make-sum exp1 exp2)
  (cond ((=number? exp1 0) exp2)
		((=number? exp2 0) exp1)
		((and (number? exp1) (number? exp2))
		 (+ exp1 exp2))
		(else (list exp1 '+ exp2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2))
		 (* m1 m2))
		(else (list m1 '* m2))))

(define (sum? exp)
  (and (pair? exp) (memq '+ exp)))

(define (addend exp)
  (if (eq? (cadr exp) '+)
	  (car exp)
	  (addend (cons (list (car exp) (cadr exp) (caddr exp))
					(cdddr exp)))))

(define (augend exp)
  (if (or (eq? (cadr exp) '+) (= 3 (length exp)))
	  (let ((rest (cddr exp)))
		(if (= 1 (length rest))
			(car rest)
			rest))
	  (augend (cons (list (car exp) (cadr exp) (caddr exp))
					(cddr exp)))))
