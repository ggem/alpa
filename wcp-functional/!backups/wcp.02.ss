;;;
;;;
;;;

(load "utils.ss")
(load "optimize.ss")

(define wcp
  (lambda (program)
    (syncase program
      [`(',body)
       `(,(wcp-body body))]
      [`((define (,var . ,args) ,body) .
	 ,rest-program)
       `((define (,var . ,args) ,(wcp-exp body)) .
	 ,(wcp rest-program))])))

(define wcp-exp
  (lambda (exp)
    (simplify (T exp))))

(define wcp-body
  (letrec ([process
	     (lambda (exp arg-count k)
	       (cond
		 [(eq? exp 'size)
		  (let ([var (make-variable 'size arg-count)])
		    (k var (+ arg-count 1) `(,var) `()))]
		 [(eq? exp 'unknown)
		  (let ([var (make-variable 'unknown arg-count)])
		    (k var (+ arg-count 1) '()
		      `((,var (make-unknown)))))]
		 [(atom? exp)
		  (k exp arg-count '() '())]
		 [(eq? (car exp) 'make-list)
		  (let ([var  (make-variable 'size arg-count)]
			[list (make-variable 'list-size arg-count)])
		    (k list (+ arg-count 1) `(,var)
		      `((,list (make-unknown-list ,var)))))]
		 [else
		   (process (car exp) arg-count
		     (lambda (car-exp arg-count args-car bindings-car)
		       (process (cdr exp) arg-count
			 (lambda (cdr-exp arg-count args-cdr bindings-cdr)
			   (k (cons car-exp cdr-exp)
			     arg-count
			     (append args-car args-cdr)
			     (append bindings-car bindings-cdr))))))]))])
    (lambda (body)
      (process body 0
	(lambda (exp arg-count args bindings)
	  `(lambda ,args
	     (let ,bindings
	       (flatten-path (caddr ,(T exp))))))))))

(define T
  (lambda (exp)
    (syncase exp
      [`,v (guard (symbol? v)) `(list ,v Tvar '())]
      [`,c (guard (atom? c)) `(list ,c Tc '())]
      [`',c `(list ',c Tc '())]
      [`(if ,exp1 ,exp2 ,exp3)
       (let ([Texp1 (T exp1)]
	     [Texp2 (T exp2)]
	     [Texp3 (T exp3)]
	     [counter (count!)])
	 (make-variables `((v- t- p- tmp-) (,counter) (-1 -2 -3))
	   (lambda (v1 v2 v3 t1 t2 t3 p1 p2 p3 tmp1 tmp2 tmp3)
	     (tree-bind v1 t1 p1 tmp1 Texp1
	       `(if (unknown? ,v1)
		  ,(tree-bind v2 t2 p2 tmp2 Texp2   v3 t3 p3 tmp3 Texp3
		     `(if (> ,t2 ,t3)
			(list (lub ,v2 ,v3) (+ Tif ,t1 ,t2)
			  (merge-paths ,p1 ,(make-path #t exp1) ,p2))
			(list (lub ,v2 ,v3) (+ Tif ,t1 ,t3)
			  (merge-paths ,p1 ,(make-path #f exp1) ,p3))))
		  (if ,v1
		    ,(tree-bind v2 t2 p2 tmp2 Texp2
		       `(list ,v2 (+ Tif ,t1 ,t2)
			  (merge-paths ,p1 ,(make-path #t exp1) ,p2)))
		    ,(tree-bind v3 t3 p3 tmp3 Texp3
		       `(list ,v3 (+ Tif ,t1 ,t3)
			  (merge-paths ,p1 ,(make-path #f exp1)
			    ,p3)))))))))]
      [`(let ((,vars ,exps) ...) ,body)
       (let ([Texps (map T exps)]
	     [counter (count!)])
	 (make-variables `((v- t- p- tmp-) (,counter))
	   (lambda (v t p tmp)
	     (make-tree-bindings vars Texps
	       (lambda (vals times paths)
		 (tree-bind v t p tmp (T body)
		   `(list ,v (+ Tlet ,@times ,t)
		      (merge-paths ,@paths ,p))))))))]
      [`(,prim . ,exps)
       (guard (primitive? prim))
       (let ([Texps (map T exps)]
	     [prim^ (if (constructor? prim) prim (make-variable prim '^))])
	 (make-tree-bindings #f Texps
	   (lambda (vals times paths)
	     `(list (,prim^ . ,vals)
		(+ ,(make-variable 'T prim) . ,times)
		(merge-paths . ,paths)))))]
      [`(,proc . ,exps)
       (let ([Texps (map T exps)]
	     [counter (count!)])
	 (make-variables `((v- t- p- tmp-) (,proc) (-) (,counter))
	   (lambda (proc-v proc-t proc-p proc-tmp)
	     (make-tree-bindings #f Texps
	       (lambda (vals times paths)
		 (tree-bind proc-v proc-t proc-p proc-tmp (cons proc vals)
		   `(list ,proc-v
		      (+ Tcall ,@times ,proc-t)
		      (merge-paths ,@paths ,proc-p))))))))])))

(define simplify (lambda (x) x))

(define scheme-wrap
  (let* ([unknown-check
	   (lambda (var)
	     `(unknown? ,var))]
	 [make-unknown-aware
	   (lambda (proc . args)
	     `(define ,(make-variable proc '^)
		(lambda ,args
		  (if ,(if (null? (cdr args))
			 (unknown-check (car args))
			 (cons 'or (map unknown-check args)))
		    (make-unknown)
		    (,proc ,@args)))))])
    (lambda (exps)
      `(define ,(make-variable 'time- (caadar exps))
	 (let ()
	   (define closure	cons)
	   (define value	car)
	   (define cost		cdr)

	   ,(make-unknown-aware 'null? 'x)
	   ,(make-unknown-aware 'car 'x)
	   ,(make-unknown-aware 'cdr 'x)
	   ,(make-unknown-aware 'eq? 'x 'y)
	   ,(make-unknown-aware '+   'x 'y)
	   ,(make-unknown-aware '-   'x 'y)
	   ,(make-unknown-aware '*   'x 'y)
	   ,(make-unknown-aware '>   'x 'y)
	   ,(make-unknown-aware '<   'x 'y)
	   ,(make-unknown-aware '=   'x 'y)
	 
	   (define lub
	     (lambda (x y)
	       (cond
		 [(equal? x y) x]
		 [(atom? x) (make-unknown)]
		 [(atom? y) (make-unknown)]
		 [(unknown? x) x] ;[(make-unknown)]
		 [(unknown? y) y] ;[(make-unknown)]
		 [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))

	   ,@exps)))))

(define primitives '(car cdr cons null? eq? + - * > < =))
(define constructors '(cons))

(define constructor?
  (lambda (name)
    (and (symbol? name) (memq name constructors))))

(define primitive?
  (lambda (name)
    (and (symbol? name) (memq name primitives))))



;;;
;;;
;;;

(define wcp-file
  (let ([read-file (lambda (filename)
		     (let ([ip (open-input-file filename)])
		       (do ([ls '() (cons x ls)]
			    [x (read ip) (read ip)])
			 [(eof-object? x) (reverse ls)])))])
    (lambda (filename)
      (wcp (read-file filename)))))

(define simp!
  (let ([id   (lambda (x) x)]
	[old-simplify simplify]
	[simplifying? #t])
    (lambda ()
      (if simplifying?
	(begin
	  (set! simplify id)
	  (set! simplifying? #f))
	(begin
	  (set! simplify old-simplify)
	  (set! simplifying? #t))))))

;;;
;;;
;;;


;(pretty-one-line-limit 90) (pretty-line-length 90)
