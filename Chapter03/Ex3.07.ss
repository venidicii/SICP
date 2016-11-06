(define (make-account inital-account init-password)
  (lambda (password dispach)
	(if (eq? init-password password)
		(lambda (amount)
		  (cond ((eq? dispach 'withdraw)
				 (if (>= inital-account amount)
					 (begin (set! inital-account (- inital-account amount))
							inital-account)
					 "Insufficient funds"))
				((eq? dispach 'deposit)
				 (begin (set! inital-account (+ inital-account amount))
						inital-account))
				(else
				 (error "Unknown request -- MAKE-ACCOUNT" dispach))))
		(begin
		  (display "Incorrect password")
		  (newline)
		  (lambda (x)
			false)))))

(define (make-joint init-account init-password new-password)
  (when ((init-account init-password 'deposit) 0)
	(lambda (password dispach)
	  (if (eq? new-password password)
		  (lambda (amount)
			(cond ((eq? dispach 'withdraw)
				   ((init-account init-password 'withdraw) amount))
				  ((eq? dispach 'deposit)
				   ((init-account init-password 'deposit) amount))
				  (else
				   (error "Unknown request -- MAKE-ACCOUNT" dispach))))
		  (error "Incorrect password")))))


;;------------ Test ------------
;;> (define acc (make-account 100 'venidici))
;;> ((acc 'venidici 'deposit) 10)
;;110
;;> ((acc 'venidici 'deposit) 10)
;;120
;;> (define bcc (make-joint acc 'venidici 'cucaracha))
;;> ((bcc 'venidici 'withdraw) 10)
;;Incorrect password
;;  context...:
;;   /usr/local/share/racket/collects/racket/private/misc.rkt:88:7
;;> ((bcc 'cucaracha 'deposit) 10)
;;130
;;> ((bcc 'cucaracha 'deposit) 10)
;;140
;;> ((bcc 'cucaracha 'withdraw) 10)
;;130
;;> ((acc 'cucaracha 'withdraw) 10)
;;Incorrect password
;;#f
;;> ((acc 'cucaracha 'deposit) 10)
;;Incorrect password
;;#f
;;> ((acc 'venidici 'deposit) 10)
;;140

