;;;
;;;
;;;


(define alpa
  (lambda (program)
    (if (null? (cdr program))
      (list (alpa-body (cadar program)))
      (let ([definition (car program)])
	(let ([var (cadr definition)]
	      [exp (caddr definition)])
	  (cons `(define ,var ,(alpa-exp exp))
	    (alpa (cdr program))))))))

(define alpa-exp
  (lambda (exp)
    (simplify (Vb (V exp)))))

(define alpa-body
  (let ([make-symbol
	  (lambda (str arg)
	    (string->symbol (format str arg)))])
    (letrec ([process
	       (lambda (exp arg-count k)
		 (cond
		   [(eq? exp 'size)
		    (let ([var (make-symbol "size~a" arg-count)])
		      (k var (+ arg-count 1) `(,var) `()))]
		   [(atom? exp)
		    (k exp arg-count '() '())]
		   [(eq? (car exp) 'make-list)
		    (let ([var  (make-symbol "size~a" arg-count)]
			  [list (make-symbol "list-size~a" arg-count)])
		      (k list (+ arg-count 1) `(,var)
			`((,list (make-list ,var)))))]
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
		 (cost-vector->exp
		   ,(simplify (Cb (C (V exp)))))))))))))

(define V
  (lambda (exp)
    (cond
      [(primitive? exp) (primitive->closure exp)]
      [(atom? exp) exp]
      [else
	(record-case exp
	  [quote (quoted-exp) exp]
	  [if (test-exp then-exp else-exp)
	    `(if ,(V test-exp) ,(V then-exp) ,(V else-exp))]
	  [let (bindings body)
	    `(let ,(map (lambda (binding)
			  (list (car binding) (V (cadr binding))))
		     bindings)
	       ,(V body))]
	  [letrec (bindings body)
	    `(letrec ,(map (lambda (binding)
			     (list (car binding) (V (cadr binding))))
			bindings)
	       ,(V body))]
	  [lambda (formals body)
	    (let ([Vbody (V body)])
	      `(closure
		 (lambda ,formals ,Vbody)
		 (lambda ,formals ,(C Vbody))))]
	  [else
	    (if (primitive? (car exp))
	      `(,(car exp) ,@(map V (cdr exp)))
	      `(value-apply ,@(map V exp)))])])))

(define C
  (lambda (exp)
    (cond
      [(symbol? exp) 'cost_varref]
      [(atom? exp) 'cost_const]
      [else
	(record-case exp
	  [quote (quoted-exp) 'cost_const]
	  [if (test-exp then-exp else-exp)
	    `(if ,test-exp
	       (c+ cost_if ,(C test-exp) ,(C then-exp))
	       (c+ cost_if ,(C test-exp) ,(C else-exp)))]
	  [let (bindings body)
	    `(let ,bindings
	       (c+ cost_let ,(C body)
		 ,@(map (lambda (x) (C (cadr x))) bindings)))]
	  [letrec (bindings body)
	    `(letrec ,bindings
	       (c+ cost_letrec ,(C body)
		 ,@(map (lambda (x) (C (cadr x))) bindings)))]
	  [value-apply (proc . args)
	    `(c+ cost_funcall (cost-apply ,proc ,@args)
	       ,(C proc) ,@(map C args))]
	  [closure (value-proc cost-proc)
	    'cost_closure]
	  [else
	    `(c+ ,(primitive->cost (car exp)) ,@(map C (cdr exp)))])])))

(define Vb
  (lambda (exp)
    (cond
      [(atom? exp) exp]
      [else
	(record-case exp
	  [quote (quoted-exp) exp]
	  [if (test-exp then-exp else-exp)
	    (let ([test-bound (Vb test-exp)]
		  [then-bound (Vb then-exp)]
		  [else-bound (Vb else-exp)])
	      `(let ([test ,test-bound])
		 (if (eq? test 'unknown)
		   (lub ,then-bound ,else-bound)
		   (if test ,then-bound ,else-bound))))]
	  [let (bindings body)
	    `(let ,(map (lambda (b) (list (car b) (Vb (cadr b))))
		     bindings)
	       ,(Vb body))]
	  [letrec (bindings body)
	    `(letrec ,(map (lambda (b) (list (car b) (Vb (cadr b))))
			bindings)
	       ,(Vb body))]
	  [value-apply (proc . args)
	    `((value ,(Vb proc)) ,@(map Vb args))]
	  [closure (value cost)
	    (if (primitive? value)
	      exp
	      `(closure
		 (lambda ,(cadr value) ,(Vb (caddr value)))
		 (lambda ,(cadr cost)  ,(Cb (caddr cost)))))]
	  [else
	    (let ([primitive (car exp)])
	      (if (constructor? primitive)
		(map Vb exp)
		(cons (primitive->prim^ primitive) (map Vb (cdr exp)))))])])))

(define Cb
  (lambda (exp)
    (cond
      [(atom? exp) exp]
      [else
	(record-case exp
	  [quote (quoted-exp) exp]
	  [if (test-exp then-exp else-exp)
	    (let ([test-bound (Vb test-exp)]
		  [then-cost  (Cb then-exp)]
		  [else-cost  (Cb else-exp)])
	      `(let ([test ,test-bound])
		 (if (eq? test 'unknown)
		   (cmax ,then-cost ,else-cost)
		   (if test ,then-cost ,else-cost))))]
	  [let (bindings body)
	    `(let ,(map (lambda (b) (list (car b) (Vb (cadr b))))
		     bindings)
	       ,(Cb body))]
	  [letrec (bindings body)
	    `(letrec ,(map (lambda (b) (list (car b) (Vb (cadr b))))
			bindings)
	       ,(Cb body))]
	  [cmax (c1 c2)
	    `(cmax ,(Cb c1) ,(Cb c2))]
	  [c+ args
	    `(c+ ,@(map Cb (cdr exp)))]
	  [cost-apply (fun . args)
	    `((cost ,(Vb fun)) ,@(map Vb args))]
	  [else
	    (error 'Cb "what function is this? ~s" exp)])])))


(define costs-names
  '(cost_car cost_cdr cost_cons cost_null cost_eq
     cost_plus cost_minus cost_times cost_greater cost_lessthan cost_equal
     cost_const cost_varref cost_if
     cost_let cost_letrec cost_funcall cost_closure))

(define simplify-add
  (let ([zero-vector (make-vector (length costs-names) 0)])
    (letrec ([add
	       (lambda (v lst non-vectors)
		 (cond
		   [(null? lst)
		    (cond
		      [(null? non-vectors) v]
		      [(eq? v zero-vector) `(c+ ,@non-vectors)]
		      [(null? (cdr non-vectors)) (nest1 v (car non-vectors))]
		      [else `(c+ ,@(nest* v non-vectors))])]
		   [(vector? (car lst))
		    (add (c+ (car lst) v) (cdr lst) non-vectors)]
		   [(and (pair? (car lst)) (eq? (caar lst) 'c+))
		    (add v (append (cdr lst) (cdar lst)) non-vectors)]
		   [else (add v (cdr lst) (cons (car lst) non-vectors))]))]
	     [nest1
	       (lambda (vec exp)
		 (record-case exp
		   [if (test-exp then-exp else-exp)
		     `(if ,test-exp
			,(add vec (list then-exp) '())
			,(add vec (list else-exp) '()))]
		   [let (bindings body)
		     `(let ,bindings ,(add vec (list body) '()))]
		   [letrec (bindings body)
		     `(letrec ,bindings ,(add vec (list body) '()))]
		   [cmax (v1 v2)
		     `(cmax ,(add vec (list v1) '()) ,(add vec (list v2) '()))]
		   [else
		     `(c+ ,vec ,exp)]))]
	     [nest*
	       (lambda (vec exps)
		 (if (null? exps)
		   (list vec)
		   (record-case (car exps)
		     [if (test-exp then-exp else-exp)
		       `((if ,test-exp
			   ,(add vec (list then-exp) '())
			   ,(add vec (list else-exp) '()))
			 ,@(cdr exps))]
		     [let (bindings body)
		       `((let ,bindings ,(add vec (list body) '()))
			 ,@(cdr exps))]
		     [letrec (bindings body)
		       `((letrec ,bindings ,(add vec (list body) '()))
			 ,@(cdr exps))]
		     [cmax (v1 v2)
		       `((cmax ,(add vec (list v1) '())
			   ,(add vec (list v2) '()))
			 ,@(cdr exps))]
		     [else
		       `(,(car exps) ,@(nest* vec (cdr exps)))])))])
      (lambda (exps)
	(add zero-vector exps '())))))

(define simplify
  (let ([costs 
	  (let ([size (length costs-names)])
	    (let ([make-cost (lambda (i)
			       (let ([v (make-vector size 0)])
				 (vector-set! v i 1)
				 v))])
	      (do ([names (reverse costs-names) (cdr names)]
		   [costs '() (cons (cons (car names) (make-cost i)) costs)]
		   [i (- size 1) (- i 1)])
		[(< i 0) costs])))])
    (lambda (exp)
      (cond
	[(assq exp costs) => cdr]	;;; get the cost vector if it exists
	[(atom? exp) exp]
	[else
	  (record-case exp
	    [quote (quoted-exp) exp]
	    [if (test-exp then-exp else-exp)
	      `(if ,(simplify test-exp)
		 ,(simplify then-exp)
		 ,(simplify else-exp))]
	    [let (bindings body)
	      `(let ,(map (lambda (b) (list (car b) (simplify (cadr b))))
		       bindings)
		 ,(simplify body))]
	    [letrec (bindings body)
	      `(letrec ,(map (lambda (b) (list (car b) (simplify (cadr b))))
			  bindings)
		 ,(simplify body))]
	    [lambda (formals body)
	      `(lambda ,formals ,(simplify body))]
	    [closure (value cost)
	      `(closure ,(simplify value) ,(simplify cost))]
	    [cmax (c1 c2)
	      (let ([c1 (simplify c1)]
		    [c2 (simplify c2)])
		(if (and (vector? c1) (vector? c2))
		  (cmax c1 c2)
		  `(cmax ,c1 ,c2)))]
	    [c+ args
	      (simplify-add (map simplify args))]
	    [else
	      (map simplify exp)])]))))


(define scheme-wrap
  (let* ([unknown-check
	   (lambda (var)
	     `(eq? ,var 'unknown))]
	 [make-unknown-aware
	   (lambda (proc . args)
	     `(lambda ,args
		(if ,(if (null? (cdr args))
		       (unknown-check (car args))
		       (cons 'or (map unknown-check args)))
		  'unknown
		  (,proc ,@args))))])
    (lambda (exps)
      `(define ,(string->symbol (format "cost-~a" (cadar exps)))
	 (let ()
	   (define closure	cons)
	   (define value	car)
	   (define cost		cdr)
	 
	   (define null?^ ,(make-unknown-aware 'null? 'x))
	   (define car^	,(make-unknown-aware 'car 'x))
	   (define cdr^	,(make-unknown-aware 'cdr 'x))
	   (define eq?^	,(make-unknown-aware 'eq? 'x 'y))
	   (define +^	,(make-unknown-aware '+   'x 'y))
	   (define -^	,(make-unknown-aware '-   'x 'y))
	   (define *^	,(make-unknown-aware '*   'x 'y))
	   (define >^	,(make-unknown-aware '>   'x 'y))
	   (define <^	,(make-unknown-aware '<   'x 'y))
	   (define =^	,(make-unknown-aware '=   'x 'y))
	 
	   (define lub
	     (lambda (x y)
	       (cond
		 [(equal? x y) x]
		 [(atom? x) 'unknown]
		 [(atom? y) 'unknown]
		 [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))])))

	   (define make-list
	     (lambda (n)
	       (if (= n 0)
		 '()
		 (cons 'unknown (make-list (- n 1))))))

	   ,@exps)))))


(define c+
  (lambda (cost0 . costs)
    (let ([answ (vector-copy cost0)]
	  [size (vector-length cost0)])
      (do ([costs costs (cdr costs)])
	[(null? costs) answ]
	(do ([i (- size 1) (- i 1)])
	  [(< i 0)]
	  (vector-set! answ i
	    (+ (vector-ref answ i) (vector-ref (car costs) i))))))))

(define cmax
  (lambda (cost1 cost2)
    (let ([size (vector-length cost1)])
      (let ([vec (make-vector size)])
	(do ([i 0 (+ i 1)])
	  [(= i size) vec]
	  (vector-set! vec i
	    (max (vector-ref cost1 i) (vector-ref cost2 i))))))))

(define c*
  (lambda (n vec)
    (list->vector
      (map (lambda (x) (* x n))
	(vector->list vec)))))

(define cost-vector->exp
  (lambda (v)
    (letrec ([loop
	       (lambda (i answ names)
		 (if (null? names)
		   (cond
		     [(null? answ) 0]
		     [(null? (cdr answ)) (car answ)]
		     [else (cons '+ answ)])
		   (let ([sym (car names)]
			 [n (vector-ref v i)])
		     (loop (+ i 1) (cond
				     [(zero? n) answ]
				     [(= n 1) (cons sym answ)]
				     [else `((* ,n ,sym) ,@answ)])
		       (cdr names)))))])
      (loop 0 '() costs-names))))


(define bindings->vars
  (lambda (bindings)
    (map car bindings)))

(define bindings->exps
  (lambda (bindings)
    (map cadr bindings)))

(define constructor?
  (lambda (name)
    (and (symbol? name) (memq name constructors))))

(define primitive?
  (lambda (name)
    (and (symbol? name) (assq name primitives))))

(define primitive->closure
  (lambda (name)
    (let ([prim (assq name primitives)])
      `(closure ,name (lambda args ,(caddr prim))))))

(define primitive->cost
  (lambda (name)
    (caddr (assq name primitives))))

(define primitive->prim^
  (lambda (name)
    (cadddr (assq name primitives))))

(define primitives
  `((car	,car	cost_car	car^)
    (cdr	,cdr	cost_cdr	cdr^)
    (cons	,cons	cost_cons	*****)
    (null?	,null?	cost_null	null?^)
    (eq?	,eq?	cost_eq		eq?^)
    (+		,+	cost_plus	+^)
    (-		,-	cost_minus	-^)
    (*		,*	cost_times	*^)
    (>		,>	cost_greater	>^)
    (<		,<	cost_lessthan	<^)
    (=		,=	cost_equal	=^)))

(define constructors
  `(cons))


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

(define Vb-known
  (lambda (exp)
    (cond
      [(atom? exp) exp]
      [else
	(record-case exp
	  [quote (quoted-exp) exp]
	  [value-apply (proc . args)
	    `((value ,(Vb-known proc)) ,@(map Vb-known args))]
	  [cost-apply (fun . args)
	    `((cost ,(Vb-known fun)) ,@(map Vb-known args))]
	  [else
	    (map Vb-known exp)])])))

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

(define nobound!
  (let ([id   Vb-known]
	[old-Vb Vb]
	[binding? #t])
    (lambda ()
      (if binding?
	(begin
	  (set! Vb id)
	  (set! binding? #f))
	(begin
	  (set! Vb old-Vb)
	  (set! binding? #t))))))


;;;
;;;
;;;


;(* 440 cost_funcall)
;(* 20 cost_binding)
;(* 861 cost_cond)
;(* 2162 cost_varref)
;(* 20 cost_constant)
;(* 400 cost_booleanop)
;(* 441 cost_null)
;(* 20 cost_cons)
;(* 420 cost_cdr)
;(* 440 cost_car)

; (eval (scheme-wrap (alpa test02)))

;(pretty-one-line-limit 90) (pretty-line-length 90)
