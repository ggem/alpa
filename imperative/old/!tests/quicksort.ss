;

(alpa-imp
  (vars a)
  (definitions
    (define (quicksort vec)
      (vars)
      (set! a vec)
      (qs 0 (- (vector-length vec) 1))
      vec)
    (define (qs low high)
      (vars pivot-index)
      (while (< low high)
	(set! pivot-index (partition low high))
	(qs low (- pivot-index 1))
	(set! low (+ pivot-index 1))))

    (define (partition low high)
      (vars left right pivot tmp)
      (set! pivot (vector-ref a low))
      (set! left low)
      (Set! right high)
      (while (< left right)
	(while (and (<= left right) (<= (vector-ref a left)  pivot))
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

    (pretty-print (quicksort '#(11 2 12 4 10 6 13 8 14 0 15 1 16 9 17 7 5 3))))
