(define (add x)
  (lambda (y) (+ x y)))
(define (one f)
  (lambda (x) (f x)))
