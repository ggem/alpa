(alpa-imp
  (vars)
  (definitions
    (define (bubble-sort array)
      (vars size i j j-1 tmp)
      (set! size (vector-length array))
      (set! i 1)
      (while (< i size)
	(set! j i)
	(set! j-1 (- j 1))
	(while (< j size)
	  (if (< (vector-ref array j) (vector-ref array j-1))
	    (begin
	      (set! tmp (vector-ref array j))
	      (vector-set! array j (vector-ref array j-1))
	      (vector-set! array j-1 tmp)))
	  (set! j (+ j 1)))
	(set! i (+ i 1)))
      (pretty-print array)))

  (bubble-sort '#(9 7 5 3 1 8 6 4 2 0)))

