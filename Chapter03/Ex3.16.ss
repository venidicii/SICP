(define (count-pairs x)
  (if (not (pair? x))
	  0
	  (+ (count-pairs (car x))
		 (count-pairs (cdr x))
		 1)))

;;------------ Result ------------
;;Return 3:
;;=> (define u '(2))
;;=> (define v (cons u '()))
;;=> (define w (cons v '()))
;;=> w
;;Value: (((2)))
;;=> (count-pairs w)
;;Value: 3

;;Return 4:
;;=> (define u '(2))
;;=> (define v (cons u '()))
;;=> (define w (cons v '()))
;;=> w
;;Value 64: (((2)))
;;=> (set-cdr! w (caar w))
;;=> w
;;Value 64: (((2)) 2)
;;=> (count-pairs w)
;;Value: 4

;;Return 7:
;;=> (define u '(2))
;;=> (define v (cons u '()))
;;=> (set-cdr! v (car v))
;;=> (define w (cons v '()))
;;=> (set-cdr! w (car w))
;;=> w
;;Value 64: (((2)) (2))
;;=> (count-pairs w)
;;Value: 7

;;Never return at all:
;;=> (define u '(2))
;;=> (define v (cons u '()))
;;=> (define w (cons v '()))
;Value: w
;;=> (set-cdr! (caar w) w)
;;=> (count-pairs w)
;;Aborting!: maximum recursion depth exceeded
