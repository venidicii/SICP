(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object )
  (eq? (car object) 'leaf))
(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
		right
		(append (symbols left)
				(symbols right))
		(+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
	  (list (symbol-leaf tree))
	  (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
	  (weight-leaf tree)
	  (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
	(if (null? bits)
		'()
		(let ((next-branch
			   (choose-branch (car bits) current-branch)))
		  (if (leaf? next-branch)
			  (cons (symbol-leaf next-branch)
					(decode-1 (cdr bits) tree))
			  (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else
		 (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
		((< (weight x) (weight (car set)))
		 (cons x set))
		(else
		 (cons (car set)
			   (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
	  '()
	  (let ((pair (car pairs)))
		(adjoin-set (make-leaf (car pair)
							   (cadr pair))
					(make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
	  '()
	  (append (encode-symbol (car message) tree)
			  (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (if (memq sym (symbols tree))
	  (if (memq sym (symbols (left-branch tree)))
		  (if (leaf? (left-branch tree))
			  (list 0)
			  (cons 0 (encode-symbol sym (left-branch tree))))
		  (if (leaf? (right-branch tree))
			  (list 1)
			  (cons 1 (encode-symbol sym (right-branch tree)))))
	  (error "bad symbol -- ENCODE-SYMBOL?" sym)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (if (= 1 (length leafs))
	  (car leafs)
	  (let ((newleafs (sort leafs (lambda (x y) (< (weight x) (weight y))))))
		(successive-merge (cons (make-code-tree (car newleafs) (cadr newleafs))
								(cddr newleafs))))))

;;------------ Something New ------------

(define test-tree
  (generate-huffman-tree
   '((A 2) (Na 16) (Boom 1) (Sha 3)
	 (Get 2) (Yip 9) (Job 2) (Wah 1))))

(define rock-song '(Get A Job Sha Na Na Na Na Na Na Na Na Get A Job Sha Na Na Na Na Na Na Na Na Wah Yip Yip Yip Yip Yip Yip Yip Yip Yip Sha Boom)) 

(define encoded-rock-song (encode rock-song test-tree))
(define decoded-rock-song (decode encoded-rock-song test-tree)) 
