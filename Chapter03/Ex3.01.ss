(define (make-accumulator addend)
  (lambda (augend)
	(begin (set! addend (+ augend addend))
		   addend)))
