;;Recursive Version
(define (square-tree tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (* x x)))
       tree))

;;Higher-order Procedures Version
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))
