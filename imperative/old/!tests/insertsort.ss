;;

(alpa-imp
  (vars)
  (definitions
    (define (insertsort a)
      (vars i j size tmp)
      (set! i 1)
      (set! size (vector-length a))
      (while (< i size)
	(set! j i)
	(while (and (> j 0) (< (vector-ref a j) (vector-ref a (- j 1))))
	  (set! tmp (vector-ref a j))
	  (vector-set! a j (vector-ref a (- j 1)))
	  (vector-set! a (- j 1) tmp)
	  (set! j (- j 1)))
	(set! i (+ i 1)))
      a))

  (pretty-print (insertsort '#(11 2 12 4 10 6 13 8 14 0 15 1 16 9 17 7 5 3))))


