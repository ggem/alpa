;;;
;;;
;;;

(define costs '(cost_addition
		 cost_binding
		 cost_booleanop
		 cost_car
		 cost_cdr
		 cost_cond
		 cost_cons
		 cost_constant
		 cost_division
		 cost_funcall
		 cost_multiplication
		 cost_negation
		 cost_null
		 cost_varref))

(define average
  (lambda (times % filter)
    (letrec ([nth-cdr
	       (lambda (n ls)
		 (if (zero? n)
		   ls
		   (nth-cdr (- n 1) (cdr ls))))]
	     [middle
	       (lambda (lst)
		 (let* ([len (length lst)]
			[cut (quotient (* len (- 100 %)) 200)])
		   (if (= % 0)
		     (if (null? lst)
		       lst
		       (list (list-ref lst (quotient len 2))))
		     (reverse (nth-cdr cut (reverse (nth-cdr cut lst)))))))]
	     [average-hlp
	       (lambda (ls n acc)
		 (cond
		   [(null? ls) (if (> n 0) (/ acc n) 0.0)]
		   [(filter (car ls))
		    (average-hlp (cdr ls) (+ n 1) 
		      (+ acc (car ls)))]
		   [else
		     (average-hlp (cdr ls) n acc)]))]
	     [average
	       (lambda (ls)
		 (if (null? ls)
		   0.0
		   (average-hlp ls 0 0.0)))])
      (average (middle (sort < times))))))

(define get-times
  (lambda (name)
    (eval (string->symbol (string-append (symbol->string name) "s")))))

(define (get-averages)
  (do ([filters (list (lambda (x) #t) (lambda (x) (>= x 0))) (cdr filters)]
       [filstrs '("all" "non-negative") (cdr filstrs)])
    [(null? filters)]
    (do ([procs (list car cdr) (cdr procs)]
	 [strs '("without" "with") (cdr strs)])
      [(null? procs)]
      (do ([% 100 (- % 10)])
	[(negative? %)]
	(printf "~n~n;;; ~a garbage collection~n" (car strs))
	(printf ";;; using ~a% of the data~n" %)
	(printf ";;; average of ~a numbers~n~n" (car filstrs))
	(do ([costs costs (cdr costs)])
	  [(null? costs)]
	  (let ([runtimes (map (car procs) (get-times (car costs)))])
	    (printf "(define ~a	~a)~n" (car costs)
	      (average runtimes % (car filters)))))))))

(get-averages)
(exit)
