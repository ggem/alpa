;;;;
;;;; scheme subset interpreter ...
;;;;
;;;;


;;
;; -- data access procedures --
;;
(define variable? symbol?)
(define literal? (lambda (x) (and (atom? x) (not (symbol? x)))))
(define app->rator car)
(define app->rands cdr)
(define decl->var  car)
(define decl->exp  cadr)


;;
;; -- primitive procedures --
;;

(define *prim-op-names* '(+ - * / add1 sub1
			   cons car cdr list null?
			   atom? pair?
			   eq? equal?
			   zero? = > <))
(define *prim-op-procs* (list
			  + - * / add1 sub1
			  cons car cdr list null?
			  atom? pair?
			  eq? equal?
			  zero? = > <))


;;
;; -- closure procedures --
;;
(define make-closure
  (lambda (formals body env)
    (lambda args
      (if (variable? formals)
	(eval-exp body (extend-env (list formals) (list args) env))
	(eval-exp body (extend-env formals args env))))))



;;
;; -- environment procedures --
;;
(define the-empty-env
  (list 'empty-env))

(define extend-env
  (lambda (sym-list val-list env)
    (list 'extended-env sym-list val-list env)))

(define apply-env
  (lambda (top-env symbol)
    (letrec
      ([apply-env
	 (lambda (env)
	   (record-case env
	     (empty-env ()
	       (error 'apply-env "no association for symbol ~s in ~s"
		 symbol top-env))
	     (extended-env (names values env)
	       (let ((x (search names values)))
		 (if x
		   (car x)
		   (apply-env env))))
	     (else (error 'apply-env "Invalid environment: ~s" env))))]
       [search (lambda (names values)
		 (cond
		   [(null? names) #f]
		   [(eq? (car names) symbol) values]
		   [else (search (cdr names) (cdr values))]))])
      (apply-env top-env))))

(define init-env 'to-be-defined)

(define reset-init-env
  (lambda ()
    (set! init-env
      (extend-env
	*prim-op-names*
	*prim-op-procs*
	the-empty-env))))

(define extend-init-env
  (lambda (name value)
    (set-car! (cdr init-env) (cons name (cadr init-env)))
    (set-car! (cddr init-env) (cons value (caddr init-env)))))


;;
;; cost-addition procedures
;;

(define indices '((cost_addition . 0)
		  (cost_binding . 1)
		  (cost_booleanop . 2)
		  (cost_car . 3)
		  (cost_cdr . 4)
		  (cost_cond . 5)
		  (cost_cons . 6)
		  (cost_constant . 7)
		  (cost_division . 8)
		  (cost_funcall . 9)
		  (cost_multiplication . 10)
		  (cost_negation . 11)
		  (cost_null . 12)
		  (cost_varref . 13)))

;; cost is a 14 elems vector
(define cost->exp
  (lambda (v)
    (letrec ([loop
	       (lambda (i answ ind)
		 (if (null? ind)
		   (cons '+ answ)
		   (let ([sym (caar ind)]
			 [n (vector-ref v i)])
		     (loop (+ i 1) (cond
				     [(zero? n) answ]
				     [(= n 1) (cons sym answ)]
				     [else `((* ,n ,sym) ,@answ)])
		       (cdr ind)))))])
      (loop 0 '() indices))))

(define symbol->cost
  (lambda (sym)
    (let ([x (assq sym indices)])
      (if x
	(let ([v (make-vector 14 0)])
	  (vector-set! v (cdr x) 1)
	  v)
	sym))))		;;; if it is not a cost_<something>, don't change it

(define new-plus
  (letrec ([add-vectors
	     (lambda (lst)
	       (let ([v (make-vector 14 0)])
		 (letrec ([add2 (lambda (v2)
				  (do ([i 0 (+ i 1)])
				    ((= i 14))
				    (vector-set! v i (+ (vector-ref v i)
						       (vector-ref v2 i)))))]
			  [add-all (lambda (lst)
				     (if (null? lst)
				       v
				       (begin (add2 (car lst))
					 (add-all (cdr lst)))))])
		   (add-all lst))))])
    (lambda args
      (cond
	[(vector? (car args)) (add-vectors args)]
	[(number? (car args)) (apply #%+ args)]
	[else (error '+ "invalid arguments: ~s" args)]))))

(define simplify-add
  (let ([zero-vector (make-vector 14 0)])
    (letrec ([add
	       (lambda (v lst non-vectors)
		 (cond
		   [(null? lst)
		    (cond
		      [(null? non-vectors) `(quote ,v)]
		      [(eq? v zero-vector) `((varref +) ,@non-vectors)]
		      [else `((varref +) (quote ,v) ,@non-vectors)])]
		   [(cost? (car lst))
		    (add (new-plus (cadar lst) v) (cdr lst) non-vectors)]
		   [else (add v (cdr lst) (cons (car lst) non-vectors))]))])
      (lambda (exp)
	(add zero-vector (cdr exp) '())))))

(define new-max
  (letrec ([max-vectors
	     (lambda (v1 v2)
	       (let ([v (make-vector 14 0)])
		 (do ([i 0 (+ i 1)])
		   ((= i 14) v)
		   (vector-set! v i (max (vector-ref v1 i)
				      (vector-ref v2 i))))))])
    (lambda (arg1 arg2)
      (cond
	[(and (vector? arg1) (vector? arg2)) (max-vectors arg1 arg2)]
	[(and (number? arg1) (number? arg2)) (#%max arg1 arg2)]
	[else (error 'max "invalid arguments: ~s ~s" arg1 arg2)]))))


;;
;; -- interpreter procedures --
;;
(define eval-exp
  (lambda (exp env)
    (record-case exp
      (varref (var) (apply-env env var))
      (quote (datum) datum)
      (if (test then else)
	(if (eval-exp test env)
	  (eval-exp then env)
	  (eval-exp else env)))
      (lambda (formals body)
	(make-closure formals body env))
      (define (var value)
	(extend-init-env var (eval-exp value env)))
      (begin (exp1 exp2)
	(eval-exp exp1 env)
	(eval-exp exp2 env))
      (else
	(let ((proc (eval-exp (app->rator exp) env))
	      (args (eval-rands (app->rands exp) env)))
	  (apply-proc proc args))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (exp) (eval-exp exp env))
      rands)))

(define apply-proc apply)



;;
;; -- expander procedure --
;;

(define addition?
  (lambda (exp)
    (and (pair? exp) (equal? (car exp) '(varref +)))))

(define cost?
  (lambda (exp)
    (and (eq? (car exp) 'quote) (vector? (cadr exp)))))

(define expand
  (letrec ([expand
	     (lambda (exp)
	       (cond
		 ((literal? exp) `(quote ,exp))
		 ((variable? exp) (let ([cost (symbol->cost exp)])
				    (if (variable? cost) ; it isn't a cost
				      `(varref ,exp)
				      `(quote ,cost))))
		 (else
		   (record-case exp
		     (varref (var) exp)
		     (quote (datum) exp)
		     (define (var value-exp)
		       `(define ,var ,(expand value-exp)))
		     (lambda (formals . bodies)
		       `(lambda ,formals ,(expand (cons 'begin bodies))))
		     (let (decls . bodies)
		       (let ((vars (map decl->var decls))
			     (exps (map decl->exp decls)))
			 `((lambda ,vars ,(expand (cons 'begin bodies)))
			   ,@(map expand exps))))
		     (begin list-of-expr
		       (cond
			 [(null? list-of-expr)
			  (error 'expand "invalid syntax: (begin)")]
			 [(null? (cdr list-of-expr))
			  (expand (car list-of-expr))]
			 [else `(begin
				  ,(expand (car list-of-expr))
				  ,(expand (cons 'begin (cdr list-of-expr))))]))
		     (cond list-of-conds
			   (if (null? list-of-conds)
			     ''void
			     (let ([first (car list-of-conds)]
				   [rest  (cdr list-of-conds)])
			       (if (eq? 'else (car first))
				 (expand (cons 'begin (cdr first)))
				 (expand `(if ,(car first)
					    (begin ,@(cdr first))
					    (cond ,@(cdr list-of-conds))))))))
		     (if (test-exp then-exp else-exp)
		       `(if ,(expand test-exp)
			  ,(expand then-exp)
			  ,(expand else-exp)))
		     (else (let ([answ (map expand exp)])
			     (if (addition? answ)
			       (simplify-add
				 (if (addition? (cadr answ))
				   (if (addition? (caddr answ))
				     `((varref +) ,@(cdadr answ) ,@(cdaddr answ))
				     `((varref +) ,@(cdadr answ) ,(caddr answ)))
				   (if (addition? (caddr answ))
				     `((varref +) ,@(cdaddr answ) ,(cadr answ))
				     answ)))
			       answ)))))))])
    expand))



;;
;; -- read-eval-print-loop procedures --
;;
(define run
  (lambda (exp)
    (reset-init-env)
    (extend-init-env '+ new-plus)
    (extend-init-env 'max new-max)
    (eval-exp (expand exp) init-env)))

(define repl ; read-eval-print loop
  (lambda ()
    (display "--> ")
    (write (run (read)))
    (newline)
    (repl)))



;;;;
;;;; end of interpreter procedures
;;;;
;;;; beginning of testing procedures
;;;;

(define test
  (lambda (n m)
    (cost->exp (run `(begin ,costbound-source (costbound ,n ,m))))))

(define varref (lambda (x) x))

(define test
  (lambda args
    (fluid-let ([+ new-plus] [max new-max])
      (cost->exp
	(eval (expand `(begin ,costbound-source (costbound ,@args))))))))


(define costbound-source '
  (define costbound
    (let ([cost_addition	1]
	  [cost_binding		1]
	  [cost_booleanop	1]
	  [cost_car		1]
	  [cost_cdr		1]
	  [cost_cond		1]
	  [cost_cons		1]
	  [cost_constant	1]
	  [cost_division	1]
	  [cost_funcall		1]
	  [cost_multiplication	1]
	  [cost_negation	1]
	  [cost_null		1]
	  [cost_varref		1]
	  [make-proc1
	    (lambda (proc)
	      (lambda (arg) (if (eq? arg 'unknown) 'unknown (proc arg))))]
	  [make-proc2
	    (lambda (proc)
	      (lambda (arg1 arg2)
		(cond
		  [(eq? arg1 'unknown) 'unknown]
		  [(eq? arg2 'unknown) 'unknown]
		  [else (proc arg1 arg2)])))])
      (define car_1 (make-proc1 car))
      (define cdr_1 (make-proc1 cdr))
      (define eq?_1 (make-proc2 eq?))
      (define <_1 (make-proc2 <))
      (define +_1 (make-proc2 +))
      (define -_1 (make-proc2 -))
      (define *_1 (make-proc2 *))
      (define /_1 (make-proc2 /))
      (define null?_1 (make-proc1 null?))
      (define lub_1
	(lambda (x y)
	  (cond
	    [(equal? x y) x]
	    [(atom? x) 'unknown]
	    [else
	      (cons (lub_1 (car x) (car y)) (lub_1 (cdr x) (cdr y)))])))
      (define length_1
	(lambda (n)
	  (if (= n 0) '() (cons 'unknown (length_1 (- n 1))))))
      (define cost_union
	(lambda (l1 l2)
	  (+ cost_cond
	    (+ (+ cost_null cost_varref)
              (let ([x:1 (null?_1 l1)])
                (if (eq? x:1 'unknown)
		  (max cost_constant
		    (let ([rr (union (cdr l1) l2)])
		      (+ cost_binding
			(+ (+ cost_funcall
			     (+ (cost_union (cdr l1) l2)
			       (+ (+ cost_cdr cost_varref)
				 cost_varref)))
			  (+ cost_cond
			    (+ (+ cost_funcall
				 (+ (cost_member? (car l1) l2)
				   (+ (+ cost_car cost_varref)
				     cost_varref)))
			      (let ([x:1 (member? (car_1 l1) l2)])
				(if (eq? x:1 'unknown)
				  (max cost_varref
				    (+ cost_cons
				      (+ (+ cost_car
					   cost_varref)
					cost_varref)))
				  (if x:1
				    cost_varref
				    (+ cost_cons
				      (+ (+ cost_car
					   cost_varref)
					cost_varref)))))))))))
		  (if x:1
		    cost_constant
		    (let ([rr (union (cdr l1) l2)])
		      (+ cost_binding
			(+ (+ cost_funcall
			     (+ (cost_union (cdr l1) l2)
			       (+ (+ cost_cdr cost_varref)
				 cost_varref)))
			  (+ cost_cond
			    (+ (+ cost_funcall
				 (+ (cost_member? (car l1) l2)
				   (+ (+ cost_car cost_varref)
				     cost_varref)))
			      (let ([x:1 (member? (car_1 l1) l2)])
				(if (eq? x:1 'unknown)
				  (max cost_varref
				    (+ cost_cons
				      (+ (+ cost_car
					   cost_varref)
					cost_varref)))
				  (if x:1
				    cost_varref
				    (+ cost_cons
				      (+ (+ cost_car
					   cost_varref)
					cost_varref)))))))))))))))))
      (define union
	(lambda (l1 l2)
	  (let ([x:1 (null?_1 l1)])
	    (if (eq? x:1 'unknown)
              (lub_1
                '()
                (let ([rr (union (cdr_1 l1) l2)])
                  (let ([x:1 (member? (car_1 l1) l2)])
                    (if (eq? x:1 'unknown)
		      (lub_1 rr (cons (car_1 l1) rr))
		      (if x:1 rr (cons (car_1 l1) rr))))))
              (if x:1
		'()
		(let ([rr (union (cdr_1 l1) l2)])
		  (let ([x:1 (member? (car_1 l1) l2)])
		    (if (eq? x:1 'unknown)
		      (lub_1 rr (cons (car_1 l1) rr))
		      (if x:1 rr (cons (car_1 l1) rr))))))))))
      (define cost_member?
	(lambda (a l)
	  (+ cost_cond
	    (+ (+ cost_null cost_varref)
              (let ([x:1 (null?_1 l)])
                (if (eq? x:1 'unknown)
		  (max cost_constant
		    (+ cost_cond
		      (+ (+ cost_booleanop
			   (+ (+ cost_car cost_varref) cost_varref))
			(let ([x:1 (eq?_1 (car_1 l) a)])
			  (if (eq? x:1 'unknown)
			    (max cost_constant
			      (+ cost_funcall
				(+ (cost_member? a (cdr l))
				  (+ cost_varref
				    (+ cost_cdr
				      cost_varref)))))
			    (if x:1
			      cost_constant
			      (+ cost_funcall
				(+ (cost_member? a (cdr l))
				  (+ cost_varref
				    (+ cost_cdr
				      cost_varref))))))))))
		  (if x:1
		    cost_constant
		    (+ cost_cond
		      (+ (+ cost_booleanop
			   (+ (+ cost_car cost_varref) cost_varref))
			(let ([x:1 (eq?_1 (car_1 l) a)])
			  (if (eq? x:1 'unknown)
			    (max cost_constant
			      (+ cost_funcall
				(+ (cost_member? a (cdr l))
				  (+ cost_varref
				    (+ cost_cdr
				      cost_varref)))))
			    (if x:1
			      cost_constant
			      (+ cost_funcall
				(+ (cost_member? a (cdr l))
				  (+ cost_varref
				    (+ cost_cdr
				      cost_varref))))))))))))))))
      (define member?
	(lambda (a l)
	  (let ([x:1 (null?_1 l)])
	    (if (eq? x:1 'unknown)
              (lub_1
                #f
                (let ([x:1 (eq?_1 (car_1 l) a)])
                  (if (eq? x:1 'unknown)
		    (lub_1 #t (member? a (cdr_1 l)))
		    (if x:1 #t (member? a (cdr_1 l))))))
              (if x:1
		#f
		(let ([x:1 (eq?_1 (car_1 l) a)])
		  (if (eq? x:1 'unknown)
		    (lub_1 #t (member? a (cdr_1 l)))
		    (if x:1 #t (member? a (cdr_1 l))))))))))
      (lambda args
	(cost_union
	  (length_1 (car args))
	  (length_1 (car (cdr args))))))))

'(do ([lst '(0 1 2 5 10 20 30 50 70 100 150 200 250 300 400 500 1000 1500) (cdr lst)])
  ((null? lst))
  (let ([n (car lst)])
    (printf "~n(costbound ~s ~s)		" n n)
    (pretty-print (time (test n n)))))
