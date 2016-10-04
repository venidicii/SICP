(define (make-center-percent c p)
  (make-interval (* c (+ 1.0 p))
                 (* c (- 1.0 p))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (percent i)
  (let ((c (center i)))
    (/ (- (upper-bound i) (lower-bound i))
       2.0 c)))
