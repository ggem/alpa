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
    (simplify `(2nd ,(T exp)))))

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
		   ,(simplify `(1st ,(T exp))))))))))))

(define T
  (lambda (exp)
    (cond
      [(primitive? exp)	(primitive->pair exp)]
      [(symbol? exp)	`(pair cost_varref ,exp)]
      [(atom? exp)	`(pair cost_const ,exp)]
      [else
	(record-case exp
	  [quote (quoted-exp) `(pair cost_const ,exp)]
	  [if (test-exp then-exp else-exp)
	    (let ([Ttest (T test-exp)]
		  [Tthen (T then-exp)]
		  [Telse (T else-exp)])
	      `(if (2nd ,Ttest)
		 (pair (c+ cost_if (1st ,Ttest) (1st ,Tthen)) (2nd ,Tthen))
		 (pair (c+ cost_if (1st ,Ttest) (1st ,Telse)) (2nd ,Telse))))]
	  [let (bindings body)
	    (let* ([Tbody (T body)]
		   [vars (map car bindings)]
		   [exps (map cadr bindings)]
		   [Texps (map T exps)]
		   [1stTexps (map (lambda (x) `(1st ,x)) Texps)]
		   [2ndTexps (map (lambda (x) `(2nd ,x)) Texps)]
		   [Tbindings (map list vars 2ndTexps)])
	      `(let ,Tbindings
		 (pair (c+ cost_let (1st ,Tbody) ,@1stTexps) (2nd ,Tbody))))]
	  [letrec (bindings body)
	    (let* ([Tbody (T body)]
		   [vars (map car bindings)]
		   [exps (map cadr bindings)]
		   [Texps (map T exps)]
		   [1stTexps (map (lambda (x) `(1st ,x)) Texps)]
		   [2ndTexps (map (lambda (x) `(2nd ,x)) Texps)]
		   [Tbindings (map list vars 2ndTexps)])
	      `(letrec ,Tbindings
		 (pair (c+ cost_letrec (1st ,Tbody) ,@1stTexps) (2nd ,Tbody))))]
	  [lambda (formals body)
	    (let ([Tbody (T body)])
	      `(pair cost_closure
		 (closure (lambda ,formals (1st ,Tbody))
		   (lambda ,formals (2nd ,Tbody)))))]
	  [else
	    (let ([rator (car exp)]
		  [rands (cdr exp)])
	      (let ([Trator (T rator)]
		    [Trands (map T rands)])
		(let* ([1stTrands (map (lambda (x) `(1st ,x)) Trands)]
		       [2ndTrands (map (lambda (x) `(2nd ,x)) Trands)])
		  (if (primitive? rator)
		    `(pair (c+ ,(primitive->cost rator) ,@1stTrands)
		       (,rator ,@2ndTrands))
		    `(pair (c+ cost_funcall (1st ,Trator) ,@1stTrands
			     ((cost (2nd ,Trator)) ,@2ndTrands))
		       ((value (2nd ,Trator)) ,@2ndTrands))))))])])))


(define simplify-pairs
  (lambda (nth exp)			; nth in {1st, 2nd}
    (letrec ([simp
	       (lambda (exp)
		 (if (atom? exp)
		   `(,nth ,exp)
		   (record-case exp
		     [if (test-exp then-exp else-exp)
		       `(if ,test-exp ,(simp then-exp) ,(simp else-exp))]
		     [let (bindings body)
		       `(let ,bindings ,(simp body))]
		     [letrec (bindings body)
		       `(letrec ,bindings ,(simp body))]
		     [pair (cost value)
		       (if (eq? nth '1st) cost value)]
		     [else
		       `(,nth ,exp)])))])
      (simp exp))))


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
	    [pair (cost-function value-function)
	      `(pair ,(simplify cost-function) ,(simplify value-function))]
	    [1st (possible-pair)
	      (let ([pp (simplify possible-pair)])
		(simplify-pairs '1st pp))]
	    [2nd (possible-pair)
	      (let ([pp (simplify possible-pair)])
		(simplify-pairs '2nd pp))]
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
	   (define closure cons)
	   (define cost    car)
	   (define value   cdr)
	 
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
		 (cons (begin 'unknown (random 100000)) (make-list (- n 1))))))

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

(define primitive->pair
  (lambda (name)
    (let ([prim (assq name primitives)])
      `(pair cost_varref (closure (lambda args ,(caddr prim)) ,name)))))

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
