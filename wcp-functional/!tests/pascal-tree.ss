(define pascal-tree
  (lambda (num-rows)
    (pascal-tree-helper num-rows 0 '())))

(define pascal-tree-helper
  (lambda (num-rows curr-row prev-rows)
    (if (= curr-row num-rows)
	prev-rows
	(let ([new-tree (pascal-row curr-row 0 prev-rows)])
	  (pascal-tree-helper num-rows 
			      (+ curr-row 1) 
			      (new-append prev-rows (cons new-tree '())))))))

(define pascal-row
  (lambda (row col prev-rows)
    (if (> col row)
	'()
	(if (or (= col 0) (= col row))
	    (cons 1 (pascal-row row (+ col 1) prev-rows))
	    (cons (+ (pascal-elem (- row 1) (- col 1) prev-rows)
		     (pascal-elem (- row 1) col prev-rows))
		  (pascal-row row (+ col 1) prev-rows))))))

(define pascal-elem
  (lambda (row col pascal-tree)
    (if (null? pascal-tree)
	#f
	(if (= row 0)
	    (new-list-ref (car pascal-tree) col)
	    (pascal-elem (- row 1) col (cdr pascal-tree))))))

(define new-append
  (lambda (ls1 ls2)
    (if (null? ls1)
	ls2
	(cons (car ls1) (new-append (cdr ls1) ls2)))))

(define new-list-ref
    (lambda (ls n)
      (if (null? ls)
	  #f
	  (if (= n 0)
	      (car ls)
	      (new-list-ref (cdr ls) (- n 1))))))
