;;;
;;;
;;;

(load "synforms.ss")
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
    (optimize
      (T exp
	(lambda (v t p wrap) (wrap `(list ,v ,t ,p)))))))

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
	       (flatten-path
		 ,(optimize (T exp  (lambda (v t p wrap) (wrap p))))))))))))

(define T
  (lambda (exp k)
    (syncase exp
      [`,v (guard (symbol? v)) (k v 'Tvar '() id)]
      [`,c (guard (atom? c)) (k c 'Tc '() id)]
      [`',c (k exp 'Tc '() id)]
      [`(if ,exp1 ,exp2 ,exp3)
       (T exp1
	 (lambda (v1 t1 p1 wrap1)
	   (T exp2
	     (lambda (v2 t2 p2 wrap2)
	       (T exp3
		 (lambda (v3 t3 p3 wrap3)
		   (make-variables `((v- t- p- tmp-) (,(count!)))
		     (lambda (v0 t0 p0 tmp0)
		       (make-binding v0 t0 p0
			 (lambda (x)
			   (wrap1
			     `(let ([,tmp0 (if (unknown? ,v1)
					     ,((compose wrap2 wrap3)
					       `(if (> ,t2 ,t3)
						  (list (lub ,v2 ,v3)
						    (+ Tif ,t1 ,t2)
						    (merge-paths ,p1
						      ,(make-path #t exp1) ,p2))
						  (list (lub ,v2 ,v3)
						    (+ Tif ,t1 ,t3)
						    (merge-paths ,p1
						      ,(make-path #f exp1) ,p3))))
					     (if ,v1
					       ,(wrap2
						  `(list ,v2 (+ Tif ,t1 ,t2)
						     (merge-paths ,p1 ,p2)))
					       ,(wrap3
						  `(list ,v3 (+ Tif ,t1 ,t3)
						     (merge-paths ,p1 ,p3)))))])
				(let ([,v0 (car ,tmp0)]
				      [,t0 (cadr ,tmp0)]
				      [,p0 (caddr ,tmp0)])
				  ,x))))
			 k)))))))))]
      [`(let ((,vars ,exps) ...) ,body)
       (map-T vars exps
	 (lambda (vals times paths wraps)
	   (T body
	     (lambda (v t p wrap)
	       (k v `(+ Tlet ,@times ,t) `(merge-paths ,@paths ,p)
		 (compose wraps wrap))))))]
      [`(,prim . ,exps)
       (guard (primitive? prim))
       (let ([prim^ (if (constructor? prim) prim (make-variable prim '^))])
	 (map-T #f exps
	   (lambda (vals times paths wrap)
	     (make-binding
	       `(,prim^ . ,vals)
	       `(+ ,(make-variable 'T prim) . ,times)
	       `(merge-paths . ,paths)
	       wrap
	       k))))]
      [`(,proc . ,exps)
       (let ([tmp (make-variable 'tmp- proc '- (count!))])
	 (map-T #f exps
	   (lambda (vals times paths wrap)
	     (make-binding
	       `(car ,tmp)
	       `(+ Tcall ,@times (cadr ,tmp))
	       `(merge-paths ,@paths (caddr ,tmp))
	       (lambda (x) (wrap `(let ([,tmp (,proc . ,vals)]) ,x)))
	       k))))])))


(define map-T
  (lambda (names exps k)
    (let loop ([vs '()]
	       [ts '()]
	       [ps '()]
	       [wraps id]
	       [exps (reverse exps)])
      (if (null? exps)
	(if names
	  (k names ts ps  (lambda (x) (wraps `(let ,(map list names vs) ,x))))
	  (k vs ts ps wraps))
	(T (car exps)
	  (lambda (v t p wrap)
	    (loop (cons v vs) (cons t ts) (cons p ps)
	      (lambda (x) (wraps (wrap x)))
	      (cdr exps))))))))


(define make-binding
  (lambda (v t p wrap k)
    (make-variables `((v- t- p-) (,(count!)))
      (lambda (tmp-v tmp-t tmp-p)
	(k tmp-v tmp-t tmp-p
	  (lambda (code)
	    (wrap `(let ([,tmp-v ,v] [,tmp-t ,t] [,tmp-p ,p]) ,code))))))))






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
	   ,(make-unknown-aware 'quotient 'x 'y)

	   (define merge-paths list)
	   (define flatten-path
	     (letrec ([flatten
			(lambda (path acc)
			  (cond
			    [(null? path) acc]
			    [(null? (car path))
			     (flatten (cdr path) acc)]
			    [(symbol? (caar path))
			     (cons (car path) (flatten (cdr path) acc))]
			    [else
			      (flatten (car path) (flatten (cdr path) acc))]))])
	       (lambda (path)
		 (flatten path '()))))

	   (define make-unknown
	     (let ([count -1])
	       (lambda ()
		 (set! count (+ count 1))
		 (cons 'unknown count))))
	   (define unknown?
	     (lambda (obj)
	       (and (pair? obj)
		    (eq? (car obj) 'unknown)
		    (number? (cdr obj)))))
	   (define make-unknown-list
	     (letrec ([make-list
			(lambda (n)
			  (if (zero? n)
			    '()
			    (cons (make-unknown) (make-list (- n 1)))))])
	       make-list))

	   (define lub
	     (lambda (x y)
	       (cond
		 [(equal? x y) x]
		 [(atom? x) (make-unknown)]
		 [(atom? y) (make-unknown)]
		 [(unknown? x) x]
		 [(unknown? y) y]
		 [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))

	   ,@exps)))))

(define primitives '(car cdr cons null? eq? + - * > < = quotient))
(define constructors '(cons))

(define constructor?
  (lambda (name)
    (and (symbol? name) (memq name constructors))))

(define primitive?
  (lambda (name)
    (and (symbol? name) (memq name primitives))))


;;;
;;; syntax makers
;;;

(define make-variable
  (let ([obj->str (lambda (obj)
		    (cond
		      [(null? obj) ""]
		      [(string? obj) obj]
		      [(number? obj) (number->string obj)]
		      [(symbol? obj) (symbol->string obj)]
		      [else (error 'make-variable "unknown type: ~a" obj)]))])
    (letrec ([make-string (lambda (objs str)
			    (if (null? objs)
			      str
			      (make-string (cdr objs)
				(string-append str (obj->str (car objs))))))])
      (lambda args
	(string->symbol (make-string args ""))))))

(define make-variables
  (let ([cross-product
	  (letrec ([cross-product-2sets
		     (lambda (set1 set2)
		       (let loop1 ([set1 set1] [answ '()])
			 (if (null? set1)
			   (reverse answ)
			   (let loop2 ([set2 set2] [answ answ])
			     (if (null? set2)
			       (loop1 (cdr set1) answ)
			       (loop2 (cdr set2)
				 `((,(car set1) . ,(car set2)) . ,answ)))))))])
	    (letrec ([cross-product
		       (lambda (set0 sets)
			 (if (null? sets)
			   (map list set0)
			   (cross-product-2sets set0
			     (cross-product (car sets) (cdr sets)))))])
	      (lambda (sets)
		(if (null? sets)
		  '()
		  (cross-product (car sets) (cdr sets))))))])
    (lambda (lists k)
      (apply k
	(map (lambda (items) (apply make-variable items))
	  (cross-product lists))))))

(define make-path
  (lambda (true? exp)
    (let ([constraint `(list ',(car exp) . ,(cdr exp))])
      (if true?
	constraint
	`(list 'not ,constraint)))))

(define count!
  (let ([counter -1])
    (lambda ()
      (set! counter (+ 1 counter))
      counter)))
;;;
;;; Times (taken from ~alpa/closure/!timing/measured-prim.txt)
;;;

(define Tcar 87.45)
(define Tcdr 60.95)
(define Tcons 95.8)
(define Tnull? 73.3)
(define Teq? 66.65)
(define T+ 66.55)
(define T- 65.15)
(define T* 509.45)
(define Tquotient 300.00) ; made up number!
(define T> 80.4)
(define T< 75.25)
(define T= 75.55)
(define Tc 0.25)
(define Tvar 2.1)
(define Tif 3.7)
(define Tlet 0.45)
(define Tcall 75.175)


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
	[old-optimize optimize]
	[optimizing? #t])
    (lambda ()
      (if optimizing?
	(begin
	  (printf "not optimizing~%")
	  (set! optimize id)
	  (set! optimizing? #f))
	(begin
	  (printf "optimizing~%")
	  (set! optimize old-optimize)
	  (set! optimizing? #t))))))

(define (id x) x)

;;;
;;;
;;;


;(pretty-one-line-limit 90) (pretty-line-length 90)
