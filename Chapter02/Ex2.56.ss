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

(define (sum? exp)
  (and (pair? exp) (eq? '+ (car exp))))

(define (make-sum exp1 exp2)
  (list '+ exp1 exp2))

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define (make-product exp1 exp2)
  (list '* exp1 exp2))

(define (addend exp)
  (cadr exp))

(define (augend exp)
  (caddr exp))

(define (multiplier exp)
  (cadr exp))

(define (multiplicand exp)
  (caddr exp))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum exp1 exp2)
  (cond ((=number? exp1 0) exp2)
		((=number? exp2 0) exp1)
		((and (number? exp1) (number? exp2))
		 (* exp1 exp2))
		(else (list '+ exp1 exp2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2))
		 (* m1 m2))
		(else (list '* m1 m2))))

(define (expontiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (exponent exp)
  (caddr exp))

(define (base exp)
  (cadr exp))

(define (make-expontiation base exp)
  (cond ((=number? exp 0)
		 (if (=number? base 0)
			 (error "Indeterminate expression 0^0 encountered.")
			 1))
		((=number? exp 1) base)
		((=number? base 0) 0)
		(else
		 (list '** base exp))))
