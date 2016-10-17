(define (enumerate-interval a b)
  (if (> a b)
	  '()
	  (cons a (enumerate-interval (+ 1 a) b))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence) (filter predicate (cdr sequence))))
		(else
		 (filter predicate (cdr sequence)))))

(define (accumulate op init seq)
  (if (null? seq)
	  init
	  (op (car seq)
		  (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (queens board-size)
  (define (queen-cols k)
	(if (= k 0)
		(list empty-board)
		(filter (lambda (position) (safe? k position))
				(flatmap
				 (lambda (rest-of-queens)
				   (map (lambda (new-row)
						  (adjoin-position new-row k rest-of-queens))
						(enumerate-interval 1 board-size)))
				 (queen-cols (- k 1))))))
  (queen-cols board-size))
