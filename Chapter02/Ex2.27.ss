(define (deep-reverse lst)
  (cond ((null? lst) '())
        ((not (pair? (car lst)))
         (append (deep-reverse (cdr lst)) (list (car lst))))
        (else
         (append (deep-reverse (cdr lst))
                 (list (deep-reverse (car lst)))))))
