;;;
;;;  newtimer-prims.ss
;;;
;;; procedures to get the time cost of scheme operations
;;;

(define timed-do
  (lambda (iterations name control exp)
    (let ([times
	    (car
	      (port->list
		(run-scheme
		  `(begin
		     (load "/u/ggomezes/alpa/functional/scheme/time.ss")
		     (define l0 (let ([lst (cons 1999 '())])
				  (set-cdr! lst lst)
				  lst))
		     (define l1 (make-list 100000 1998))
		     (define l1 (append! l1 l1))
		     (define g1 (lambda (x) (car x)))
		     (define g2 (lambda (x y) (eq? x y)))
		     (let ([start (run-time)])
		       (do ([i 0 (fx+ i 1)] . ,control)
			 ((fx= i ,iterations)))
		       (let ([middle (run-time)])
			 (do ([i 0 (fx+ i 1)] . ,exp)
			   ((fx= i ,iterations)))
			 (let ([end (run-time)])
			   (printf "~s~n"
			     (time-multiply
			       (time-minus (time-add start end)
				 (time-add middle middle))
			       (/ 1000000.0 ,iterations)))
			   (exit))))
		     (printf "error")
		     (exit)))))])
      (printf "; ~s ==> ~s~n" name times)
      times)))

(define-syntax do-tests
  (let ([names '(+ bind eq car cdr iff cons const / funcall * ! null varref)]
	[cost_names '(cost_addition cost_binding cost_booleanop cost_car
		       cost_cdr cost_cond cost_cons cost_constant
		       cost_division cost_funcall cost_multiplication
		       cost_negation cost_null cost_varref)])
    (letrec ([name->index
	       (lambda (name)
		 (letrec ([name->index
			    (lambda (names n)
			      (cond
				[(null? names) #f]
				[(eq? (car names) name) n]
				[else (name->index (cdr names) (+ n 1))]))])
		   (name->index names 0)))]
	     [syntax-names
	       (lambda (x)
		 (map (lambda (name) (datum->syntax-object x name))
		   cost_names))]
	     [syntax->indices
	       (lambda (lst)
		 (map (lambda (syntax)
			(name->index (syntax-object->datum syntax)))
		   lst))])
      (lambda (x)
	(syntax-case x ()
	  [(_ loops iterations (name1 exp1.1 exp1.2) ...)
	   (fixnum? (syntax-object->datum (syntax loops)))
	   (with-syntax ([(index1 ...) (syntax->indices (syntax (name1 ...)))]
			 [(ordered-name1 ...) (syntax-names (syntax x))]
			 [(iota1 ...) (iota 14)])
	     (syntax
	       (let ([vec (make-vector 14 '())])
		 (do ([i 0 (fx+ i 1)])
		   ((fx= i loops))
		   (vector-set! vec index1
		     (cons (timed-do iterations 'name1 'exp1.1 'exp1.2)
		       (vector-ref vec index1))) ...)
		 (printf "~%; AVERAGES~%~%")
		 (printf "(define ~ss '~s)~n" 'ordered-name1
		   (vector-ref vec iota1))
		 ...
		 (newline)
		 (printf "(define ~s	~s) ; nanoseconds~%" 'ordered-name1
		   (time->runtime (car (average (vector-ref vec iota1)))))
		 ...
		 vec)))])))))


(define iota
  (letrec ([iota (lambda (n answ)
		   (if (zero? n)
		     answ
		     (iota (- n 1) (cons (- n 1) answ))))])
    (lambda (n)
      (iota n '()))))

(define average
  (letrec ([average
	     (lambda (ls)
	       (if (null? ls)
		 0.0
		 (/ (apply + (map (lambda (x) (max 0 x)) ls))
		   (length ls))))])
    (lambda (times)
      (let ([runtimes (map time->runtime times)]
	    [runtimes+gc (map time->runtime+gc times)])
	(list (cons (average runtimes) (average runtimes+gc))
	  times)))))



(define timer-prims
  (lambda ()
    (do-tests 100 10000000
      [const	([v l0 (cdr l0)]) ([v l0 (begin (cdr l0) '())])]
      [varref	([v l0 (cdr l0)]) ([v l0 (begin (cdr l0) v)])]
      [cdr	([v l0 v]) ([v l1 (cdr v)])]
      [car	([v l0 (cdr v)] [f #t f])
    		([v l1 (cdr v)] [f #t (car v)])]
      [cons	([v l1 (cdr v)] [f #f f])
		([v l1 (cdr v)] [f #f (begin (cons f v) f)])]
      [eq	([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
		([v l1 (cdr v)] [f 0 (eq? (car v) (cadr v))])]
      [null	([v l1 (cdr v)] [f 0 (car v)])
		([v l1 (cdr v)] [f 0 (null? (car v))])]
      [+	([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
		([v l1 (cdr v)] [f 0 (+ (car v) (cadr v))])]
      [*	([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
		([v l1 (cdr v)] [f 0 (* (car v) (cadr v))])]
      [/	([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
		([v l1 (cdr v)] [f 0 (/ (car v) (cadr v))])]
      [!	([v l1 (cdr v)] [f 0 (car v)])
		([v l1 (cdr v)] [f 0 (not (car v))])]
      [bind	([v l1 (cdr v)] [f 0 (begin (car v) (boolean? f))])
		([v l1 (cdr v)] [f 0 (let ([x (car v)]) (boolean? x))])]
      [iff	([v l1 (cdr v)] [f 0 (begin (number? f) (car v))])
		([v l1 (cdr v)] [f 0 (if (number? f) (car v) (cadr v))])]
      [iff	([v l1 (cdr v)] [f 0 (begin (pair? f) (car v))])
		([v l1 (cdr v)] [f 0 (if (pair? f) (cadr v) (car v))])]
      [funcall	([v l1 (cdr v)] [f 0 (car v)])
		([v l1 (cdr v)] [f 0 (g1 v)])]
      [funcall	([v l1 (cdr v)] [f 0 (eq? (car v) (cadr v))])
		([v l1 (cdr v)] [f 0 (g2 (car v) (cadr v))])])))

(load "/u/ggomezes/alpa/functional/scheme/time.ss")
(timer-prims)
(exit)
