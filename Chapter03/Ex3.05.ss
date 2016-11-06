(define (square x) (* x x))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)
		   (/ trials-passed trials))
		  ((experiment)
		   (iter (- trials-remaining 1) (+ trials-passed 1)))
		  (else
		   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)  
  (define (P)
	(let ((x (random-in-range x1 x2))
		  (y (random-in-range y1 y2)))
	  (<= (+ (square (- x 5))
			 (square (- y 7)))
		  9)))
  (exact->inexact
   (* (monte-carlo trials P)
	  (* (- x2 x1) (- y2 y1)))))

(define (random-in-range low high)
  (+ low
	 (* (exact->inexact (/ (random 100000) 100000))
		(- high low))))

(define pi-estimate
  (lambda (trials)
	(/ (estimate-integral P 2.0 8.0 4.0 10.0 trials) 9)))

;;------------ Usage ------------
;;(time (pi-estimate 100000))
