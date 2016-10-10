;;A concise but slow version, wfs
(define (fringe lst)
  (apply append
         (map (lambda (x)
                (if (pair? x)
                    (fringe x)
                    (list x)))
              lst)))

;; A fast version, dfs
(define (fringe lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else
         (append (fringe (car lst))
                 (fringe (cdr lst))))))

;;Use common lisp can write a very simple version
;;(defun fringe (x)
;;  (if (atom x)
;;	  (list x)
;;	  (mapcan #'flatten x)))
