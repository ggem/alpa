;;;
;;;
;;;


(define alpa
  (lambda (program)
    (if (null? (cdr program))
      (list (alpa-body (car program)))
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
		   ,(simplify (C (V exp))))))))))))

(define V
  (lambda (exp)
    (cond
      [(primitive? exp) (primitive->closure exp)]
      [(atom? exp) exp]
      [else
	(record-case exp
	  [quote (quoted-exp) exp]
	  [if (test-exp then-exp else-exp)
	    `(if ,test-exp ,(V then-exp) ,(V else-exp))]
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
	  [lambda (formals body)
	    `(lambda ,formals ,(Vb body))]
	  [closure (value cost)
	    `(closure ,(Vb value) ,(Cb cost))]
	  [else
	    (let ([function (car exp)])
	      (if (and (primitive? function) (not (constructor? function)))
		(cons (primitive->prim^ function) (map Vb (cdr exp)))
		(map Vb exp)))])])))

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
	  [lambda (formals body)
	    `(lambda ,formals ,(Cb body))]
	  [closure (value cost)
	    (error 'Cb "closure? I think this shouldn't happen")]
	  [cmax (c1 c2)
	    `(cmax ,(Cb c1) ,(Cb c2))]
	  [c+ args
	    `(c+ ,@(map Cb (cdr exp)))]
	  [cost-apply (fun . args)
	    `(cost-apply ,fun ,@(map Vb args))]
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
	   (define closure     (lambda (value cost) (cons value cost)))
	   (define value-apply (lambda (f . args) (apply (car f) args)))
	   (define cost-apply  (lambda (f . args) (apply (cdr f) args)))
	 
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
      `(closure ,(cadr prim) ,(caddr prim)))))

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

(define test01
  '((define map
      (lambda (f ls)
	(if (null? ls)
	  '()
	  (cons (f (car ls)) (map f (cdr ls))))))
    (define c-add
      (lambda (n)
	(lambda (m)
	  (+ n m))))
    (lambda (n)
      (cost-vector->exp
	(cost-apply map (value-apply c-add 4) (make-list n))))))

(define test02
  '((define union
      (lambda (set1 set2)
	(if (null? set1)
	  set2
	  (let ([rr (union (cdr set1) set2)])
	    (if (member? (car set1) set2)
	      rr
	      (cons (car set1) rr))))))
    (define member?
      (lambda (item ls)
	(if (null? ls)
	  #f
	  (if (eq? item (car ls))
	    #t
	    (member? item (cdr ls))))))
    (lambda (n m)
      (cost-vector->exp (cost-apply union (make-list n) (make-list m))))))

(define test03
  '((define split
      (lambda (ls pred k)
	(if (null? ls)
	  (k '() '())
	  (split (cdr ls) pred
	    (lambda (passed failed)
	      (if (pred (car ls))
		(k (cons (car ls) passed) failed)
		(k passed (cons (car ls) failed))))))))
    (define interval-tester
      (lambda (x)
	(if (> 10 x)
	  #f
	  (if (> x 20)
	    #f
	    #t))))
    (define initial-k
      (lambda (good bad)
	(cons good (cons bad '()))))
    (lambda (n)
      (cost-vector->exp
	(cost-apply split (make-list n) interval-tester initial-k)))))

(define test04
  '((define append-cps
      (lambda (ls1 ls2 k)
	(if (null? ls1)
	  (k ls2)
	  (append-cps (cdr ls1) ls2
	    (lambda (v)
	      (k (cons (car ls1) v)))))))
    (define reverse-cps
      (lambda (ls k)
	(if (null? ls)
	  (k '())
	  (reverse-cps (cdr ls)
	    (lambda (v)
	      (append-cps v (cons (car ls) '()) k))))))
    (define reverse
      (lambda (ls)
	(reverse-cps ls (lambda (x) x))))
    (lambda (n)
      (cost-vector->exp
	(cost-apply reverse (make-list n))))))

(define test05
  '((define ackermann
      (lambda (m n)
	(if (= m 0)
	  (+ n 1)
	  (if (= n 0)
	    (ackermann (- m 1) 1)
	    (ackermann (- m 1) (ackermann m (- n 1)))))))
    (lambda (m n)
      (cost-vector->exp
	(cost-apply ackermann m n)))))

(define test06
  '((define ackermann
      (letrec ([ack (lambda (m)
		      (if (= m 0)
			(lambda (n) (+ n 1))
			(let ([ack_m-1 (ack (- m 1))])
			  (letrec ([ack_m
				     (lambda (n)
				       (if (= n 0)
					 (ack_m-1 1)
					 (ack_m-1 (ack_m (- n 1)))))])
			    ack_m))))])
	(lambda (m n)
	  ((ack m) n))))
    (lambda (m n)
      (cost-vector->exp
	(cost-apply ackermann m n)))))

(define test01
  '((define map
      (lambda (f ls)
	(if (null? ls)
	  '()
	  (cons (f (car ls)) (map f (cdr ls))))))
    (define c-add
      (lambda (n)
	(lambda (m)
	  (+ n m))))
    (map (c-add 4) (make-list))))

(define test02
  '((define union
      (lambda (set1 set2)
	(if (null? set1)
	  set2
	  (let ([rr (union (cdr set1) set2)])
	    (if (member? (car set1) set2)
	      rr
	      (cons (car set1) rr))))))
    (define member?
      (lambda (item ls)
	(if (null? ls)
	  #f
	  (if (eq? item (car ls))
	    #t
	    (member? item (cdr ls))))))
    (union (make-list) (make-list))))

(define test03
  '((define split
      (lambda (ls pred k)
	(if (null? ls)
	  (k '() '())
	  (split (cdr ls) pred
	    (lambda (passed failed)
	      (if (pred (car ls))
		(k (cons (car ls) passed) failed)
		(k passed (cons (car ls) failed))))))))
    (define interval-tester
      (lambda (x)
	(if (> 10 x)
	  #f
	  (if (> x 20)
	    #f
	    #t))))
    (define initial-k
      (lambda (good bad)
	(cons good (cons bad '()))))
    (split (make-list) interval-tester initial-k)))

(define test04
  '((define append-cps
      (lambda (ls1 ls2 k)
	(if (null? ls1)
	  (k ls2)
	  (append-cps (cdr ls1) ls2
	    (lambda (v)
	      (k (cons (car ls1) v)))))))
    (define reverse-cps
      (lambda (ls k)
	(if (null? ls)
	  (k '())
	  (reverse-cps (cdr ls)
	    (lambda (v)
	      (append-cps v (cons (car ls) '()) k))))))
    (define reverse
      (lambda (ls)
	(reverse-cps ls (lambda (x) x))))
    (reverse (make-list))))


(define test05
  '((define ackermann
      (lambda (m n)
	(if (= m 0)
	  (+ n 1)
	  (if (= n 0)
	    (ackermann (- m 1) 1)
	    (ackermann (- m 1) (ackermann m (- n 1)))))))
    (ackermann size size)))

(define test06
  '((define ackermann
      (letrec ([ack (lambda (m)
		      (if (= m 0)
			(lambda (n) (+ n 1))
			(let ([ack_m-1 (ack (- m 1))])
			  (letrec ([ack_m
				     (lambda (n)
				       (if (= n 0)
					 (ack_m-1 1)
					 (ack_m-1 (ack_m (- n 1)))))])
			    ack_m))))])
	(lambda (m n)
	  ((ack m) n))))
    (ackermann size size)))


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
