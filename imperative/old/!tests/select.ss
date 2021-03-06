;

(alpa-imp
  (vars a k)
  (definitions
    (define (select array k)
      (vars low high middle)
      (set! a array)
      (set! low 0)
      (set! high (- (vector-length a) 1))
      (set! middle -1)
      (while (not (= k middle))
	(set! middle (partition low high))
	(if (> k middle)
	  (set! low (+ middle 1))
	  (set! high (- middle 1))))
      (vector-ref a k))
    (define (partition low high)
      (vars left right pivot tmp)
      (set! pivot (vector-ref a low))
      (set! left low)
      (Set! right high)
      (while (< left right)
	(while (and (< left right) (<= (vector-ref a left) pivot))
          (set! left (+ left 1)))
	(while (> (vector-ref a right) pivot)
	  (set! right (- right 1)))
	(if (< left right)
	  (begin
	    (set! tmp (vector-ref a left))
	    (vector-set! a left (vector-ref a right))
	    (vector-set! a right tmp))))
      (vector-set! a low (vector-ref a right))
      (vector-set! a right pivot)
      right))

  (let ([ls '(11 2 12 4 10 6 13 8 14 0 15 1 16 9 17 7 5 3)])
    (pretty-print
      (map (lambda (k) (select (list->vector ls) k))
	'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))))

  (let ([v '#(11 2 12 4 10 6 13 8 14 0 15 1 16 9 17 7 5 3)])
    (select v 11)
    (select v 5)
    (select v 15)
    (pretty-print v)))
