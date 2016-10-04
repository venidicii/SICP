(define (get-power num base)
  (define (iter rest result)
    (if (= (remainder rest base) 0)
        (iter (/ rest base) (+ result 1))
        result))
  (iter num 0))

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car x)
  (get-power x 2))

(define (cdr y)
  (get-power y 3))
