;;;;
;;;; expander
;;;;

(define addition?
  (lambda (exp)
    (and (pair? exp) (equal? (car exp) '(varref +)))))

(define cost?
  (lambda (exp)
    (and (eq? (car exp) 'quote) (vector? (cadr exp)))))

(define begin?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'begin))))

(define lambda?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'lambda))))

(define literal?
  (lambda (x)
    (and (atom? x) (not (symbol? x)))))

(define variable?
  (lambda (x)
    (symbol? x)))

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

(define +^
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
		    (add (+^ (cadar lst) v) (cdr lst) non-vectors)]
		   [else (add v (cdr lst) (cons (car lst) non-vectors))]))])
      (lambda (exp)
	(add zero-vector (cdr exp) '())))))

(define max^
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
;; -- expander procedure --
;;

(define my-expand
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
		       (let ((vars (map car decls))
			     (exps (map cadr decls)))
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
		     (else
		       (let ([answ (map expand exp)])
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


;;;
;;; unexpand
;;;


(define unexpand
  (letrec ([unexpand
	     (lambda (exp)
	       (record-case exp
		 (varref (var) (cond [(assq var '((+ . +^) (max . max^)))
				      => (lambda (v) (cdr v))]
				     [else var]))
		 (quote (datum)
		   (if (or (number? datum) (vector? datum)) datum exp))
		 (define (var value-exp)
		   `(define ,var ,(unexpand value-exp)))
		 (lambda (formals body)
		   (let ([body (unexpand body)])
		     (if (begin? body)
		       `(lambda ,formals ,@(cdr body))
		       `(lambda ,formals ,body))))
		 (begin (exp1 exp2)
		   (let ([exp1^ (unexpand exp1)]
			 [exp2^ (unexpand exp2)])
		     (if (begin? exp1^)
		       (if (begin? exp2^)
			 `(begin ,@(cdr exp1^) ,@(cdr exp2^))
			 `(begin ,@(cdr exp1^) ,exp2^))
		       (if (begin? exp2^)
			 `(begin ,exp1^ ,@(cdr exp2^))
			 `(begin ,exp1^ ,exp2^)))))
		 (if (test-exp then-exp else-exp)
		   `(if ,(unexpand test-exp)
		      ,(unexpand then-exp)
		      ,(unexpand else-exp)))
		 (else
		   (let ([answ (map unexpand exp)])
		     (if (lambda? (car answ))
		       `(let ,(map list (cadar answ) (cdr answ))
			  ,@(cddar answ))
		       answ)))))])
    unexpand))

;;;; beginning of testing procedures
;;;;

(define expand-file
  (lambda (filename)
    (with-input-from-file filename
      (lambda ()
	(unexpand (my-expand (read)))))))