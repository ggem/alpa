;;;
;;;  timing.ss
;;;
;;; procedures to get the time cost of scheme operations
;;;


(define tests
  '([car	([v l0 (cdr v)] [f #t f])
		([v l1 (cdr v)] [f #t (car v)])]
    [cdr	([v l0 v]) ([v l1 (cdr v)])]
    [cons	([v l1 (cdr v)] [f #f f])
		([v l1 (cdr v)] [f #f (begin (cons f v) f)])]
    [null?	([v l1 (cdr v)] [f 0 (car v)])
      		([v l1 (cdr v)] [f 0 (null? (car v))])]
    [eq?	([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
      		([v l1 (cdr v)] [f 0 (eq? (car v) (cadr v))])]
    [+		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
      		([v l1 (cdr v)] [f 0 (+ (car v) (cadr v))])]
    [-		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
      		([v l1 (cdr v)] [f 0 (- (car v) (cadr v))])]
    [*		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
      		([v l1 (cdr v)] [f 0 (* (car v) (cadr v))])]
    [>		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
      		([v l1 (cdr v)] [f 0 (> (car v) (cadr v))])]
    [<		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
      		([v l1 (cdr v)] [f 0 (< (car v) (cadr v))])]
    [=		([v l1 (cdr v)] [f 0 (begin  (car v) (cadr v))])
      		([v l1 (cdr v)] [f 0 (= (car v) (cadr v))])]
    [const	([v l0 (cdr l0)]) ([v l0 (begin (cdr l0) '())])]
    [varref	([v l0 (cdr l0)]) ([v l0 (begin (cdr l0) v)])]
    [if		([v l1 (cdr v)] [f 0 (begin (number? f) (car v))])
      		([v l1 (cdr v)] [f 0 (if (number? f) (car v) (cadr v))])]
    [if		([v l1 (cdr v)] [f 0 (begin (pair? f) (car v))])
      		([v l1 (cdr v)] [f 0 (if (pair? f) (cadr v) (car v))])]
    [let	([v l1 (cdr v)] [f 0 (begin (car v) (boolean? f))])
      		([v l1 (cdr v)] [f 0 (let ([x (car v)]) (boolean? x))])]
    [letrec	([v l1 (cdr v)] [f 0 (begin (car v) (boolean? f))])
      		([v l1 (cdr v)] [f 0 (letrec ([x (car v)]) (boolean? x))])]
    [funcall	([v l1 (cdr v)] [f 0 (car v)])
      		([v l1 (cdr v)] [f 0 (g1 v)])]
    [funcall	([v l1 (cdr v)] [f 0 (eq? (car v) (cadr v))])
      		([v l1 (cdr v)] [f 0 (g2 (car v) (cadr v))])]
    [closure	([v l1 (cdr v)] [f 0 #f])
      		([v l1 (cdr v)] [f 0 (lambda (x y) (+ x y local1 local2))])]))


(define start-up-code
  '(begin
     (define make-infinite
       (lambda (top)
	 (letrec ([make-infinite
		    (lambda (lst)
		      (if (null? (cdr lst))
			(set-cdr! lst top)
			(make-infinite (cdr lst))))])
	   (make-infinite top)
	   top)))
     (define cpu-time
       (lambda ()
	 (let ([stt (statistics)])
	   (let ([cpu-time (vector-ref stt 1)]
		 [gc-time  (vector-ref stt 5)])
	     (- cpu-time gc-time)))))
     (define l0 (make-infinite (list 0)))
     (define l1 (make-infinite (make-list 100000 1998)))
     (define f1 (lambda (x) x))
     (define f2 (lambda (x y) x))
     (define g1 (lambda (x) (car x)))
     (define g2 (lambda (x y) (eq? x y)))
     (define-syntax timed-do
       (lambda (x)
	 (syntax-case x ()
	   [(_ iterations
	      [(var1 init1 update1) ...]
	      [(var2 init2 update2) ...])
	    (fixnum? (syntax-object->datum (syntax iterations)))
	    (syntax
	      (begin
		(collect)
		(let ([start (cpu-time)])
		  (do ([i 0 (fx+ i 1)] [var1 init1 update1] ...)
		    ((fx= i iterations)))
		  (let ([middle (cpu-time)])
		    (do ([i 0 (fx+ i 1)] [var2 init2 update2] ...)
		      ((fx= i iterations)))
		    (let ([end (cpu-time)])
		      (let ([time (max 0
				    (* (- (+ start end) (+ middle middle))
				      (/ 1000000.0 iterations)))])
			(printf " nanoseconds: ~a~%" time)))))))])))))



(define names
  '(car cdr cons null? eq? + - * > < = const
     varref if let letrec funcall closure))

(define cost_names
  '(cost_car cost_cdr cost_cons cost_null cost_eq cost_plus cost_minus
     cost_times cost_greater cost_lessthan cost_equal cost_const cost_varref
     cost_if cost_let cost_letrec cost_funcall cost_closure))

(define list-index
  (lambda (item ls)
    (letrec ([index (lambda (ls answ)
		      (cond
			[(null? ls) -1]
			[(eq? item (car ls)) answ]
			[else (index (cdr ls) (+ answ 1))]))])
      (index ls 0))))

(let ([loops 4]
      [iterations 10000000])
  (let ([vec (make-vector (length names) '())])
    (do ([i loops (- i 1)])
      [(zero? i)]
      (for-each
	(lambda (test)
	  (let* ([name (car test)]
		 [index (list-index name names)]
		 [cost (list-ref cost_names index)])
	    (printf "; ~a ==> " name)
	    (flush-output-port (current-output-port))
	    (let* ([scheme (process "exec scheme")]
		   [ip (car scheme)]
		   [op (cadr scheme)])
	      (write start-up-code op)
	      (fprintf op
		"(begin
		   (let ([local1 #t] [local2 #f]) (timed-do ~s ~s ~s))
		   (flush-output-port (current-output-port))
		   (exit))"
		iterations (cadr test) (caddr test))
	      (close-output-port op)
	      (do ([x (read ip) (read ip)])
		[(eq? x 'nanoseconds:) 'done])
	      (let ([time (read ip)])
		(printf "~a nanoseconds.~%" time)
		(vector-set! vec index (cons time (vector-ref vec index))))
	      (close-input-port ip))))
	tests))
    (for-each
      (lambda (times name)
	(printf "(define ~a ~a)~%" name (/ (apply + times) (length times))))
      (vector->list vec)
      cost_names)))

(exit)

#!eof



;;
;; uses large programs to use cache (uses instruction cache though)
;;

(define-syntax big-call
  (lambda (x)
    (syntax-case x ()
      [(_ depth exp)
       (fixnum? (syntax-object->datum (syntax depth)))
       (let ([depth (syntax-object->datum (syntax depth))])
	 (if (= depth 0)
	   (syntax exp)
	   (with-syntax ([depth-1 (datum->syntax-object (syntax x)
				    (- depth 1))])
	     (with-syntax ([call (syntax (_ depth-1 exp))])
	       (syntax
		 (begin call call))))))])))


(define-syntax make-timer
  (let ([max-loops 20])			; actually, 2^24
    (lambda (x)
      (syntax-case x ()
	[(_ times exp)
	 (let ([%times (syntax-object->datum (syntax times))])
	   (and (fixnum? %times) (<= 1 %times 24)))
	 (let ([%times (syntax-object->datum (syntax times))]
	       [->syntax (lambda (obj) (datum->syntax-object (syntax x) obj))])
	   (with-syntax ([total-loops (->syntax (/ (expt 2 max-loops) 1000.0))]
			 [small-loops (->syntax (expt 2 (- max-loops %times)))])
	     (syntax
	       (lambda ()
		 (let ([start (cpu-time)])
		   (do ([loops small-loops (fx- loops 1)])
		     [(fx= loops 0)
		      (let ([stop (cpu-time)])
			(/ (- stop start) total-loops))]
		     (big-call times exp)))))))]))))


(define time-+
  (lambda ()
    (let ([init (cpu-time)])
      (do ([loops 1000 (fx- loops 1)])
	[(= loops 0) (/ (- (cpu-time) init) 256.0)]
	(big-call 8 (+ 303030 404040))))))

;

(list
  ((make-timer 01 (* 202020 404040)))
  ((make-timer 02 (* 202020 404040)))
  ((make-timer 03 (* 202020 404040)))
  ((make-timer 04 (* 202020 404040)))
  ((make-timer 05 (* 202020 404040)))
  ((make-timer 06 (* 202020 404040)))
  ((make-timer 07 (* 202020 404040)))
  ((make-timer 08 (* 202020 404040)))
  ((make-timer 09 (* 202020 404040)))
  ((make-timer 10 (* 202020 404040)))
  ((make-timer 11 (* 202020 404040)))
  ((make-timer 12 (* 202020 404040))))
