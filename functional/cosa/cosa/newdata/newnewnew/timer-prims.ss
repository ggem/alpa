;;;
;;;  timing.ss
;;;
;;; procedures to get the time cost of scheme operations
;;;


(define time->runtime car)
(define time->runtime+gc cdr)
(define make-time cons)

(define run-time		;;; returns a pair (program . program+gc)
  (lambda ()
    (let ([stt (statistics)])
      (let ([cpu-time (vector-ref stt 1)]
	    [gc-time (vector-ref stt 5)])
	(make-time (- cpu-time gc-time) cpu-time)))))

(define time-add
  (lambda (t1 t2)
    (make-time
      (+ (time->runtime t1)    (time->runtime t2))
      (+ (time->runtime+gc t1) (time->runtime+gc t2)))))

(define time-minus
  (lambda (t1 t2)
    (make-time
      (- (time->runtime t1)    (time->runtime t2))
      (- (time->runtime+gc t1) (time->runtime+gc t2)))))

(define time-divide
  (lambda (t n)
    (make-time (/ (time->runtime t) n) (/ (time->runtime+gc t) n))))

(define time-multiply
  (lambda (t n)
    (make-time (* (time->runtime t) n) (* (time->runtime+gc t) n))))


(define-syntax timed-do
  (lambda (x)
    (syntax-case x ()
      [(_ name iterations
	 [(var1 init1 update1) ...]
	 [(var2 init2 update2) ...])
       (fixnum? (syntax-object->datum (syntax iterations)))
       (syntax
	 (let ([start (run-time)])
	   (do ([i 0 (fx+ i 1)] [var1 init1 update1] ...)
	     ((fx= i iterations)))
	   (let ([middle (run-time)])
	     (do ([i 0 (fx+ i 1)] [var2 init2 update2] ...)
	       ((fx= i iterations)))
	     (let ([end (run-time)])
	       (let ([time (time-multiply
			     (time-minus (time-add start end)
				(time-add middle middle))
			     (/ 1000000.0 iterations))])
		 (printf "; ~s ==> ~s nanoseconds~n" 'name time)
		 time)))))])))

(define-syntax do-tests
  (let ([names '(+ bind eq car cdr iff cons const / funcall * ! null varref)])
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
		   names))]
	     [syntax->indices
	       (lambda (lst)
		 (map (lambda (syntax)
			(name->index (syntax-object->datum syntax)))
		   lst))])
      (lambda (x)
	(syntax-case x ()
	  [(_ loops iterations (name1 exp1.1 exp1.2) ...)
	   (and (fixnum? (syntax-object->datum (syntax iterations)))
		(fixnum? (syntax-object->datum (syntax loops))))
	   (with-syntax ([(index1 ...) (syntax->indices (syntax (name1 ...)))]
			 [(ordered-name1 ...) (syntax-names (syntax x))]
			 [(iota1 ...) (iota 14)])
	     (syntax
	       (let ([vec (make-vector 14 '())])
		 (do ([i 0 (fx+ i 1)])
		   ((fx= i loops))
		   (vector-set! vec index1
		     (cons (timed-do name1 iterations exp1.1 exp1.2)
		       (vector-ref vec index1))) ...)
		 (printf "~n~n;AVERAGES:~n~n")
		 (printf "(define cost_~ss '~s)~n" 'ordered-name1
		   (vector-ref vec iota1)) ...
		 '(printf "(define cost_~s ~s)~n" 'ordered-name1
		   (average (vector-ref vec iota1))) ...)))])))))


(define iota
  (letrec ([iota (lambda (n answ)
		   (if (zero? n)
		     answ
		     (iota (- n 1) (cons (- n 1) answ))))])
    (lambda (n)
      (iota n '()))))

(define average
  (letrec ([nth-cdr
	     (lambda (n ls)
	       (if (zero? n)
		 ls
		 (nth-cdr (- n 1) (cdr ls))))]
	   [real-average-hlp
	     (lambda (ls n acc)
	       (if (null? ls)
		 (if (zero? n)
		   0.0
		   (/ acc n))
		 (real-average-hlp (cdr ls) (+ n 1) (+ acc (car ls)))))]
	   [real-average
	     (lambda (ls)
	       (real-average-hlp ls 0 0.0))])
    (lambda (x)
      (let* ([sorted (sort < x)]
	     [size (length sorted)]
	     [10% (quotient size 10)]
	     [chopped (reverse (nth-cdr 10% (reverse (nth-cdr 10% sorted))))])
	(real-average x)))))

(define make-infinite
  (lambda (top)
    (letrec ([make-infinite
	       (lambda (lst)
		 (if (null? (cdr lst))
		   (set-cdr! lst top)
		   (make-infinite (cdr lst))))])
      (make-infinite top)
      top)))

(define l1 (make-infinite (make-list 100000 1998)))
(define f1 (lambda (x) x))
(define f2 (lambda (x y) x))
(define g1 (lambda (x) (null? x)))
(define g2 (lambda (x y) (eq? x y)))

(do-tests 100 2000000
  [const	([v l1 (null? v)]) ([v l1 (begin (pair? v) '())])]
  [varref	([v l1 (null? v)]) ([v l1 (begin (pair? v) v)])]
  [cdr		([v l1 v]) ([v l1 (cdr v)])]
  [car		([v l1 (cdr v)] [f #t f])
    		([v l1 (cdr v)] [f #t (car v)])]
  [cons		([v l1 (cdr v)] [f #f f])
		([v l1 (cdr v)] [f #f (begin (cons f v) f)])]
  [eq		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
		([v l1 (cdr v)] [f 0 (eq? (car v) (cadr v))])]
  [null		([v l1 (cdr v)] [f 0 (car v)])
		([v l1 (cdr v)] [f 0 (null? (car v))])]
;  [+		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
;		([v l1 (cdr v)] [f 0 (+ (car v) (cadr v))])]
;  [*		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
;		([v l1 (cdr v)] [f 0 (* (car v) (cadr v))])]
;  [/		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
;		([v l1 (cdr v)] [f 0 (/ (car v) (cadr v))])]
;  [!		([v l1 (cdr v)] [f 0 (car v)])
;		([v l1 (cdr v)] [f 0 (not (car v))])]
  [bind		([v l1 (cdr v)] [f 0 (begin (car v) (boolean? f))])
		([v l1 (cdr v)] [f 0 (let ([x (car v)]) (boolean? x))])]
  [iff		([v l1 (cdr v)] [f 0 (begin (number? f) (car v))])
		([v l1 (cdr v)] [f 0 (if (number? f) (car v) #f)])]
  [iff		([v l1 (cdr v)] [f 0 (begin (pair? f) (car v))])
		([v l1 (cdr v)] [f 0 (if (pair? f) 0 (car v))])]
  [funcall	([v l1 (cdr v)] [f 0 (begin (null? v) v)])
		([v l1 (cdr v)] [f 0 (g1 v)])]
  [funcall	([v l1 (cdr v)] [f 0 (begin (car v) (cadr v) v)])
		([v l1 (cdr v)] [f 0 (g2 (car v) (cadr v))])])
