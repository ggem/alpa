;;

(alpa-imp
  (vars)
  (definitions
    (define (selectsort a)
      (vars i j n tmp min-index)
      (set! i 0)
      (set! n (vector-length a))
      (while (< i (- n 1))
	(set! min-index i)
	(set! j (+ i 1))
	(while (< j n)
	  (if (< (vector-ref a j) (vector-ref a min-index))
	    (set! min-index j))
	  (set! j (+ j 1)))
	(set! tmp (vector-ref a min-index))
	(vector-set! a min-index (vector-ref a i))
	(vector-set! a i tmp)
	(set! i (+ i 1)))
      a))
  (pretty-print (selectsort '#(11 2 12 4 10 6 13 8 14 0 15 1 16 9 17 7 5 3))))
