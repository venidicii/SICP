(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;;------------ Something New ------------

(define (successive-merge leafs)
  (if (= 1 (length leafs))
	  (car leafs)
	  (let ((newleafs (sort leafs (lambda (x y) (< (weight x) (weight y))))))
		(successive-merge (cons (make-code-tree (car newleafs) (cadr newleafs))
								(cddr newleafs))))))

