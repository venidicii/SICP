(define (cons x y)
  (λ (m) (m x y)))

(define (car z)
  (z (λ (p q) p)))

(define (cdr z)
  (z (λ (p q) q)))
