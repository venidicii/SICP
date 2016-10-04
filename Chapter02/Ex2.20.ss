(define (same-parity . lst)
  (define (select sel rest result)
    (cond ((null? rest) result)
          ((sel (car rest))
           (select sel (cdr rest) (cons (car rest) result)))
          (else
           (select sel (cdr rest) result))))
  (reverse
   (if (even? (car lst))
       (select even? lst null)
       (select odd? lst null))))
