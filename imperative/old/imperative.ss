;;;
;;;
;;;

(load "synforms.ss")

(define alpa
  (lambda (program)
    ;; program has the form ('(var-decls) (define (f . args) body) ... 'body)
    (cons (car program)			; ignore the var declarations
      (let loop ([pgm (cdr program)])
	(syncase pgm
	  [`(',body) `(,(alpa-body body))]
	  [`((define (,var . ,args) ,body) . ,rest)
	   `((define (,var . ,args) ,(alpa-exp body)) ,(loop rest))])))))

(define alpa-exp
  (lambda (exp)
    (T exp 'initial-env 'initial-store
      (lambda (v t e s w) (w `(list ,v ,t ,e ,s))))))

(define alpa-body
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

(define newT
  (let ()
    (define T
      (lambda (exp)
        (syncase exp
          [`,v (guard (symbol? v)) `(lambda (env sto) (tuple (env ,v) Tvar env sto))]
          [`,c (guard (atom? c)) `(lambda (env sto) (tuple ,c Tc env sto))]
          [`',c `(lambda (env sto) (tuple ',c Tc env sto))]
          [`(set! ,var ,exp)
           (make-variables `((v- t-) (,(count!)))
             (lambda (v1 t1)
               `(lambda (env sto)
                  (let ([<,v1 ,t1 env sto> (,(T exp) env sto)])
                    (tuple ,v1 (+ ,t1 Tassign) (env:update ',var ,v1 env) sto)))))]
          [`(begin ,exp1 ,exp2)
           (make-variables `((v- t-) (,(count!)) (-1 -2))
             (lambda (v1 v2 t1 t2)
               `(lambda (env sto)
                  (let ([<,v1 ,t1 env sto> (,(T exp1) env sto)])
                    (let ([<,v2 ,t2 env sto> (,(T exp2) env sto)])
                      (tuple ,v2 (+ ,t1 ,t2 Tseq) env sto))))))]
          [`(let ([,var ,exp1]) ,exp2)
           (make-variables `((v- t-) (,(count!)) (-1 -2))
             (lambda (v1 v2 t1 t2)
               `(lambda (env sto)
                  (let ([<,v1 ,t1 env sto> (,(T exp1) env sto)])
                    (let ([<,v2 ,t2 env sto> (,(T exp2)
                                              (env:extend ',var ,v1 env)
                                              sto)])
                      (tuple ,v2 (+ ,t1 ,t2 Tlet) env sto))))))]
          [`(if ,exp1 ,exp2 ,exp3)
           (make-variables `((v- t- env- sto-) (,(count!)) (-1 -2 -3))
             (lambda (v1 v2 v3 t1 t2 t3 env1 env2 env3 sto1 sto2 sto3)
               (let ([Texp2 (T exp2)]
                     [Texp3 (T exp3)])
                 `(lambda (env sto)
                    (let ([<,v1 ,t1 env sto> (,(T exp1) env sto)])
                      (if (unknown? ,v1)
                          (let ([<,v2 ,t2 ,env2 ,sto2> (,Texp2 env sto)]
                                [<,v3 ,t3 ,env3 ,sto3> (,Texp3 env sto)])
                            (tuple (lub ,v2 ,v3)
                              (+ Tif ,t1 (max ,t2 ,t3))
                              (lub-env ,env2 ,env3)
                              (lub-store ,sto2 ,sto3)))
                          (if ,v1
                              (let ([<,v2 ,t2 env sto> (,Texp2 env sto)])
                                (tuple ,v2 (+ Tif ,t1 ,t2) env sto))
                              (let ([<,v3 ,t3 env sto> (,Texp3 env sto)])
                                (tuple ,v3 (+ Tif ,t1 ,t3) env sto)))))))))]
          [`(while ,exp1 ,exp2)
           (make-variables `((v- t- env- sto-) (,(count!)) (-) (0 1 2))
             (lambda (v0 v1 v2 t0 t1 t2 env0 env1 env2 sto0 sto1 sto2)
               `(lambda (env sto)
                  (let ([<,v0 ,t0 env sto> (,(T exp1) env sto)])
                    (begin
                      (while ,v0
                        (let ([<,v2 ,t2 ,env2 ,sto2 > (,(T exp2) env sto)])
                          (let ([<,v1 ,t1 ,env1 ,sto1 > (,(T exp1) ,env2 ,sto2)])
                            (begin
                              (set! ,v0 ,v1)
                              (begin
                                (set! ,t0 (+ ,t0 ,t1 ,t2 Tloop))
                                (begin
                                  (set! env ,env1)
                                  (set! sto ,sto1)))))))
                      (if (unknown? ,v0)
                          (abort infinity)
                          (tuple 'void (+ Twhile ,t0) env sto)))))))]
          [`(,prim . ,exps)
           (guard (primitive? prim))
           (mapT exps prim '())]
          [`(,proc . ,exps)
           (mapT exps proc '(Tcall))])))
    (define (mapT exps proc cost)
      (let ([counter (list (count!))]
            [indices (iota (add1 (length exps)))])
        (make-variables `((v-) ,counter (-) ,indices)
          (lambda all-vars
            (make-variables `((t-) ,counter (-) ,indices)
              (lambda all-times
                `(lambda (env sto)
                   ,(let loop ([vars (cdr all-vars)]
                               [times (cdr all-times)]
                               [exps exps])
                      (if (null? exps)
                          `(let ([<,(car all-vars) ,(car all-times) env sto>
                                   (,proc ,@(cdr all-vars) env sto)])
                             (tuple ,(car all-vars)
                               (+ ,@cost ,@all-times) env sto))
                          `(let ([<,(car vars) ,(car times) env sto>
                                   (,(T (car exps)) env sto)])
                             ,(loop (cdr vars) (cdr times) (cdr exps))))))))))))
    T))

(define T
  (lambda (exp e s k)
    (syncase exp
      [`,v (guard (symbol? v)) (k v 'Tvar e s id)]
      [`,c (guard (atom? c)) (k c 'Tc e s id)]
      [`',c (k exp 'Tc e s id)]
      [`(set! ,var ,exp)
       (T exp e s
	 (lambda (v t e2 s2 w)
	   (k v `(+ ,t Tset) `(extend-env ,var ,v ,e2) s2 w)))]
      [`(begin . ,exps)
       (map-T exps e s
	 (lambda (vals times e^ s^ wrap)
	   (k (car (reverse vals)) `(+ Tseq . ,times) e^ s^ wrap)))]
      [`(let ([var exp1]) exp2)
       (T exp1 e s
         (lambda (v1 t1 e1 s1 w1)
           (T exp2 e1 s1
             (lambda (v2 t2 e2 s2 w2)
               v2))))]
      [`(if ,exp1 ,exp2 ,exp3)
       (T exp1 e s
	 (lambda (v1 t1 e1 s1 w1)
	   (T exp2 e1 s1
	     (lambda (v2 t2 e2 s2 w2)
	       (T exp3 e1 s1
		 (lambda (v3 t3 e3 s3 w3)
		   (make-variables `((v- t- e- s- tmp-) (,(count!)))
		     (lambda (v0 t0 e0 s0 tmp0)
		       (make-binding v0 t0 e0 s0
			 (lambda (x)
			   (w1
			     `(let ([,tmp0 (if (unknown ,v1)
                                               ,((compose w2 w3)
                                                 `(list (lub ,v2 ,v3)
                                                    (+ Tif (+ ,t1 (max ,t2 ,t3)))
                                                    (lube ,e2 ,e3)
                                                    (lubs ,s2 ,s3)))
                                               (if ,v1
                                                   ,(w2 `(list ,v2 (+ Tif (+ ,t1 ,t2)) ,e2 ,s2))
                                                   ,(w3 `(list ,v3 (+ Tif (+ ,t1 ,t3)) ,e3 ,s3))))])
				(let ([,v0 (car ,tmp0)]
				      [,t0 (cadr ,tmp0)]
				      [,e0 (caddr ,tmp0)]
				      [,s0 (cadddr ,tmp0)])
				  ,x))))
			 k)))))))))]
      [`(,prim . ,exps)
       (guard (primitive? prim))
       (if (constructor? prim)
           (map-T exps e s
             (lambda (vals times e s wrap)
               (let ([location (make-variable 'location- (count!))])
                 (make-binding
                   location
                   `(+ ,(make-variable 'T prim) . ,times)
                   e
                   `(alloc (,location . (,prim . ,vals)) ,s)
                   (lambda (x) (wrap `(let ([,location (fresh-location!)]) ,x)))
                   k))))
           (let ([prim^ (make-variable prim '^)])
             (map-T exps e s
               (lambda (vals times e s wrap)
                 (make-binding
                   `(,prim^ . ,vals)
                   `(+ ,(make-variable 'T prim) . ,times)
                   e
                   s
                   (lambda (x) (wrap `(let ([location (fresh-location!)]) ,x)))
                   k)))))]
      [`(,proc . ,exps)
       (let ([tmp (make-variable 'tmp- proc '- (count!))])
	 (map-T exps e s
	   (lambda (vals times e s wrap)
	     (make-binding
	       `(car ,tmp)
	       `(+ Tcall ,@times (cadr ,tmp))
	       e
	       s
	       (lambda (x) (wrap `(let ([,tmp (,proc . ,vals)]) ,x)))
	       k))))])))


(define map-T
  (lambda (exps e s k)
    (let loop ([vs '()]
	       [ts '()]
	       [e e]
	       [s s]
	       [wraps id]
	       [exps exps])
      (if (null? exps)
	(k (reverse vs) (reverse ts) e s wraps)
	(T (car exps) e s
	  (lambda (v t e^ s^ wrap)
	    (loop (cons v vs) (cons t ts) e^ s^
	      (lambda (x) (wraps (wrap x)))
	      (cdr exps))))))))


(define make-binding
  (lambda (v t e s wrap k)
    (make-variables `((v- t- e- s-) (,(count!)))
      (lambda (tmp-v tmp-t tmp-e tmp-s)
	(k tmp-v tmp-t tmp-e tmp-s
	  (lambda (code)
	    (wrap `(let ([,tmp-v ,v] [,tmp-t ,t] [,tmp-e ,e] [,tmp-s ,s]) ,code))))))))






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

(define constructors '(cons vector))
(define mutators '(set-car! set-cdr! vector-set!))
(define primitives (append constructors mutators '(car cdr null? eq? + - * > < =)))

(define constructor?
  (lambda (name)
    (and (symbol? name) (memq name constructors))))

(define mutator?
  (lambda (name)
    (and (symbol? name) (memq name mutators))))

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
  (let ([make-unquote
	  (lambda (x)
	    `(,'unquote ,x))])
    (lambda (true? exp)
      (syncase exp
	[`(,prim . ,args)
	 (guard (primitive? prim))
	 (if true?
	   `(,'quasiquote (,prim . ,(map make-unquote args)))
	   `(,'quasiquote (not (,prim . ,(map make-unquote args)))))]
	[else '()]))))

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

(define alpa-file
  (let ([read-file (lambda (filename)
		     (let ([ip (open-input-file filename)])
		       (do ([ls '() (cons x ls)]
			    [x (read ip) (read ip)])
			 [(eof-object? x) (reverse ls)])))])
    (lambda (filename)
      (alpa (read-file filename)))))

(define optimize append)
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

(define test
  (lambda (exp)
    (pretty-print (T exp '() '()
		    (lambda (v t e s w)
		      (w (list v t e s)))))
    (void)))

;;;
;;;
;;;


;(pretty-one-line-limit 90) (pretty-line-length 90)
