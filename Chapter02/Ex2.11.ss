(define (make-interval x y)
  (cons (min x y)
        (max x y)))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (positive-interval? x)
  (> (lower-bound x) 0))

(define (negative-interval? x)
  (< (upper-bound x) 0))

(define (spans-zero? x)
  (not (or (positive-interval? x)
           (negative-interval? x))))

(define (mul-interval x y)
  (cond ((and (positive-interval? x) (positive-interval? y));; Case: 1
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (positive-interval? x) (spans-zero? y));; Case: 2
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (positive-interval? x) (negative-interval? y));; Case: 3
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (upper-bound y))))
        ((and (spans-zero? x) (positive-interval? y));; Case: 4
         (mul-interval y x))
        ((and (spans-zero? x) (spans-zero? y));; Case: 5
         (make-interval (min (* (lower-bound x) (upper-bound y))
                             (* (upper-bound x) (lower-bound y)))
                        (max (* (lower-bound x) (lower-bound y))
                             (* (upper-bound x) (upper-bound y)))))
        ((and (spans-zero? x) (negative-interval? y));; Case: 6
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (negative-interval? x) (positive-interval? y));; Case: 7
         (mul-interval y x))
        ((and (negative-interval? x) (spans-zero? y));; Case: 8
         (mul-interval y x))
        ((and (negative-interval? x) (negative-interval? y));; Case: 9
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))))
