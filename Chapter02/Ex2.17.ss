(define (last-pair list)
  (let ((next (cdr list)))
    (if (pair? next)
        (last-pair next)
        list)))