;;;
;;;
;;;


(define alpa
  (lambda (exps)
    (if (null? (cdr exps))
      exps
      (cons (simplify (alpa-bound (alpa-closure (car exps))))
	(alpa (cdr exps))))))

(define alpa-closure
  (lambda (exp)
    (cond
      [(primitive? exp) (primitive->closure exp)]
      [(atom? exp) exp]
      [else
	(record-case exp
	  [quote (quoted-exp) exp]
	  [if (test-exp then-exp else-exp)
	    (map alpa-closure exp)]
	  [let (bindings body)
	    `(let ,(map (lambda (binding)
			  (list (car binding) (alpa-closure (cadr binding))))
		     bindings)
	       ,(alpa-closure body))]
	  [letrec (bindings body)
	    `(letrec ,(map (lambda (binding) (list (car binding)
			       (alpa-closure (cadr binding))))
			bindings)
	       ,(alpa-closure body))]
	  [lambda (formals body)
	    `(closure
	       (lambda ,formals ,(alpa-closure body))
	       (lambda ,formals ,(alpa-cost body)))]
	  [define (var val)
	    `(define ,var ,(alpa-closure val))]
	  [else
	    (if (primitive? (car exp))
	      `(,(car exp) ,@(map alpa-closure (cdr exp)))
	      `(val-apply ,@(map alpa-closure exp)))])])))

(define alpa-cost
  (lambda (exp)
    (cond
      [(symbol? exp) 'cost_varref]
      [(atom? exp) 'cost_const]
      [else
	(record-case exp
	  [quote (quoted-exp) 'cost_const]
	  [if (test-exp then-exp else-exp)
	    `(if ,(alpa-closure test-exp)
	       (c+ cost_if ,(alpa-cost test-exp) ,(alpa-cost then-exp))
	       (c+ cost_if ,(alpa-cost test-exp) ,(alpa-cost else-exp)))]
	  [let (bindings body)
	    `(let ,(map (lambda (binding)
			  (list (car binding) (alpa-closure (cadr binding))))
		     bindings)
	       (c+ cost_let ,(alpa-cost body)
		 ,@(map (compose alpa-cost cadr) bindings)))]
	  [letrec (bindings body)
	    `(letrec ,(map (lambda (binding)
			     (list (car binding)
			       (alpa-closure (cadr binding))))
			bindings)
	       (c+ cost_letrec ,(alpa-cost body)
		 ,@(map (compose alpa-cost cadr) bindings)))]
	  [lambda (formals body)
	    'cost_closure]
	  [else
	    (if (primitive? (car exp))
	      `(c+ ,(primitive->cost (car exp)) ,@(map alpa-cost (cdr exp)))
	      `(c+ cost_funcall (cost-apply ,(alpa-closure (car exp))
				  ,@(map alpa-closure (cdr exp)))
		 ,@(map alpa-cost exp)))])])))

(define alpa-bound
  (lambda (exp)
    (cond
      [(atom? exp) exp]
      [else
	(record-case exp
	  [quote (quoted-exp) exp]
	  [if (test-exp then-exp else-exp)
	    (let ([test-bound (alpa-bound test-exp)]
		  [then-bound (alpa-bound then-exp)]
		  [else-bound (alpa-bound else-exp)])
	      `(let ([test ,test-bound])
		 (if (eq? test 'unknown)
		   (lub ,then-bound ,else-bound)
		   (if test ,then-bound ,else-bound))))]
	  [let (bindings body)
	    `(let ,(map (lambda (b) (list (car b) (alpa-bound (cadr b))))
		     bindings)
	       ,(alpa-bound body))]
	  [lambda (formals body)
	    `(lambda ,formals ,(alpa-bound body))]
	  [closure (value cost)
	    `(closure ,(alpa-bound value) ,(alpa-bound-cost cost))]
	  [else
	    (let ([function (car exp)])
	      (if (and (primitive? function) (not (constructor? function)))
		(cons (primitive->prim^ function) (map alpa-bound (cdr exp)))
		(map alpa-bound exp)))])])))

(define alpa-bound-cost
  (lambda (exp)
    (cond
      [(atom? exp) exp]
      [else
	(record-case exp
	  [quote (quoted-exp) exp]
	  [if (test-exp then-exp else-exp)
	    (let ([test-bound (alpa-bound test-exp)]
		  [then-cost  (alpa-bound-cost then-exp)]
		  [else-cost  (alpa-bound-cost else-exp)])
	      `(let ([test ,test-bound])
		 (if (eq? test 'unknown)
		   (cmax ,then-cost ,else-cost)
		   (if test ,then-cost ,else-cost))))]
	  [let (bindings body)
	    `(let ,(map (lambda (b) (list (car b) (alpa-bound (cadr b))))
		     bindings)
	       ,(alpa-bound-cost body))]
	  [lambda (formals body)
	    `(lambda ,formals ,(alpa-bound-cost body))]
	  [closure (value cost)
	    (error 'alpa-bound-cost "closure? I think this shouldn't happen")]
	  [cmax (c1 c2)
	    `(cmax ,(alpa-bound-cost c1) ,(alpa-bound-cost c2))]
	  [c+ args
	    `(c+ ,@(map alpa-bound-cost (cdr exp)))]
	  [cost-apply (fun . args)
	    `(cost-apply ,fun ,@(map alpa-bound args))]
	  [else
	    (error 'alpa-bound-cost "what function is this? ~s" exp)])])))


(define simplify-add
  (let ([zero-vector (make-vector 17 0)])
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
		     [cmax (v1 v2)
		       `((cmax ,(add vec (list v1) '())
			   ,(add vec (list v2) '()))
			 ,@(cdr exps))]
		     [else
		       `(,(car exps) ,@(nest* vec (cdr exps)))])))])
      (lambda (exps)
	(add zero-vector exps '())))))

(define simplify
  (let ([costs '(
		 (cost_car .     #17(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
		 (cost_cdr .     #17(0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
		 (cost_cons .    #17(0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
		 (cost_null .    #17(0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
		 (cost_eq .      #17(0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
		 (cost_plus .    #17(0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0))
		 (cost_minus .   #17(0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0))
		 (cost_times .   #17(0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0))
		 (cost_greater . #17(0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0))
		 (cost_equal .   #17(0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0))
		 (cost_const .   #17(0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0))
		 (cost_varref .  #17(0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0))
		 (cost_if .      #17(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0))
		 (cost_let .	 #17(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))
		 (cost_letrec .  #17(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0))
		 (cost_funcall . #17(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
		 (cost_closure . #17(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))])
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
  (lambda (exps)
    `(define cost
       (let ()
	 (define closure    (lambda (value cost) (cons value cost)))
	 (define val-apply  (lambda (f . args) (apply (car f) args)))
	 (define cost-apply (lambda (f . args) (apply (cdr f) args)))
	 
	 (define car^	(lambda (x) (if (eq? x 'unknown) x (car x))))
	 (define cdr^	(lambda (x) (if (eq? x 'unknown) x (cdr x))))
	 (define null?^	(lambda (x) (if (eq? x 'unknown) x (null? x))))
	 (define eq?^	(lambda (x y)
			  (if (or (eq? x 'unknown) (eq? y 'unknown))
			    'unknown
			    (eq? x y))))
	 (define +^	(lambda (x y)
			  (if (or (eq? x 'unknown) (eq? y 'unknown))
			    'unknown
			    (+ x y))))
	 (define -^	(lambda (x y)
			  (if (or (eq? x 'unknown) (eq? y 'unknown))
			    'unknown
			    (- x y))))
	 (define *^	(lambda (x y)
			  (if (or (eq? x 'unknown) (eq? y 'unknown))
			    'unknown
			    (* x y))))
	 (define >^	(lambda (x y)
			  (if (or (eq? x 'unknown) (eq? y 'unknown))
			    'unknown
			    (> x y))))
	 (define =^	(lambda (x y)
			  (if (or (eq? x 'unknown) (eq? y 'unknown))
			    'unknown
			    (= x y))))
	 
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

	 ,@exps))))


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
  (let ([costs-names '(cost_car cost_cdr cost_cons cost_null cost_eq
			cost_plus cost_minus cost_times cost_greater cost_equal
			cost_const cost_varref cost_if
			cost_let cost_letrec cost_funcall cost_closure)])
    (lambda (v)
      (letrec ([loop
		 (lambda (i answ names)
		   (if (null? names)
		     (cons '+ answ)
		     (let ([sym (car names)]
			   [n (vector-ref v i)])
		       (loop (+ i 1) (cond
				       [(zero? n) answ]
				       [(= n 1) (cons sym answ)]
				       [else `((* ,n ,sym) ,@answ)])
			 (cdr names)))))])
	(loop 0 '() costs-names)))))


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
    (=		,=	cost_equal	=^)))

(define constructors
  `(cons))

(define compose
  (letrec ([loop (lambda (lst answ)
		   (if (null? lst)
		     answ
		     (loop (cdr lst) ((car lst) answ))))])
    (lambda list-of-functions
      (let ([funcs (reverse list-of-functions)])
        (lambda (x)
          (loop funcs x))))))


;;;
;;;
;;;

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
      (cost-vector->exp (cost-apply map (val-apply c-add 4) (make-list n))))))

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

(define simplify* (lambda (x) x))
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
