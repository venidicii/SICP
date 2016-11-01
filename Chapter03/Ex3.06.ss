(define rand
  (let ((seed random-init))
	(lambda (operation)
	  (cond ((eq? operation 'generate)
			 (set! seed (rand-update seed))
			 seed)
			((eq? operation 'reset)
			 (lambda (x)
			   (set! seed x)))
			(else
			 (error "Unknown request -- RAND" operation))))))

;;------------ Test ------------

(define random-init 0)
(define (rand-update x) (+ x 1))

;;> (rand 'generate) 
;;  1 
;;> (rand 'generate) 
;;  2 
;;> ((rand 'reset) 0) 
;;  0 
;;> (rand 'generate) 
;;  1
