(define (reverse lst)
  (define (iter rest result)
    (if (null? rest)
        result
        (iter (cdr rest) (cons (car rest) result))))
  (iter lst null))
