;;

(alpa-imp
  (vars a n heapsize)
  (definitions
    (define (heapsort array)
      (vars root)
      (set! a array)
      (set! n (vector-length a))
      (set! heapsize n)
      ;; construct initial heap
      (set! root (quotient n 2))
      (while (>= root 0)
	(adjust root)
	(set! root (- root 1)))
      ;; sort the rest
      (set! root (- n 1))
      (while (>= root 0)
	(set! heapsize (- heapsize 1))
	(swap 0 root)
	(adjust 0)
	(set! root (- root 1)))
      a)
    (define (swap x y)
      (vars tmp)
      (set! tmp (vector-ref a x))
      (vector-set! a x (vector-ref a y))
      (vector-set! a y tmp))
    (define (adjust root)
      (vars child1 child2 big-child)
      (set! child1 (+ (+ root root) 1))
      (set! child2 (+ child1 1))
      (set! value-root (vector-ref a root))
      (set! value-child1
	(if (< child1 heapsize)
	  (vector-ref a child1)
	  value-root))
      (set! value-child2
	(if (< child2 heapsize)
	  (vector-ref a child2)
	  value-root))
      (if (> value-child1 value-root)
	(if (> value-child1 value-child2)
	  (begin (swap child1 root) (adjust child1))
	  (begin (swap child2 root) (adjust child2)))
	(if (> value-child2 value-root)
	  (begin (swap child2 root) (adjust child2))))))
  (pretty-print (heapsort '#(11 2 12 4 10 6 13 8 14 0 15 1 16 9 17 7 5 3))))

