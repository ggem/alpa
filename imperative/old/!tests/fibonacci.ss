;;
;;

(alpa-imp
  (vars)
  (definitions
    (define (fib n)
      (vars fn fn-1 fn-2)
      (if (< n 2)
	n
	(begin
	  (set! fn 1)
	  (set! fn-1 1)
	  (set! fn-2 0)
	  (set! n (- n 2))
	  (while (> n 0)
	    (set! n (- n 1))
	    (set! fn-2 fn-1)
	    (set! fn-1 fn)
	    (set! fn (+ fn-1 fn-2)))
	  fn))))
  (pretty-print
    (map fib '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))))

