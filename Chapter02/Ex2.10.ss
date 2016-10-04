(define (div-interval x y)
  (if (> (* x y) 0)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y)))
                    (make-interval (/ 1.0 (lower-bound y))))
      (error "Interval spans zero. ")))

