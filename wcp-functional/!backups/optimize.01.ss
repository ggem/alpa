;;;
;;;
;;;

(define optimize
  (let ()

;;;
;;; primitives
;;;

(define (prim:car obj)
  (syncase obj
    [`(marker pair ,the-car ,the-cdr) the-car]
    [`(marker ,x . ,y) (error 'car "not a pair: ~a" obj)]
    [else `(car ,obj)]))

(define (prim:cdr obj)
  (syncase obj
    [`(marker pair ,the-car ,the-cdr) the-cdr]
    [`(marker ,x . ,y) (error 'cdr "not a pair: ~a" obj)]
    [else `(cdr ,obj)]))

(define (prim:cons obj1 obj2)
  `(marker pair ,obj1 ,obj2))

(define (prim:null? obj)
  (syncase obj
    [`(marker null '()) `(marker boolean #t)]
    [`(marker pair . ,ignored) `(marker boolean #f)]
    [`(marker . ,ignored) (error 'null? "not a list: ~a" obj)]
    [else `(null? ,obj)]))

(define (prim:< obj1 obj2)
  (syncase obj1
    [`(marker number ,x)
     (syncase obj2
       [`(marker number ,y) `(marker boolean ,(< x y))]
       [`(+ (marker number ,y) . ,z)
	(if (< x y)
	  `(marker boolean #t)
	  `(< ,obj1 ,obj2))]
       [else `(< ,obj1 ,obj2)])]
    [`(+ (marker number ,x) . ,z)
     (syncase obj2
       [`(marker number ,y)
	(if (< y x)
	  `(marker bookean #f)
	  `(< ,obj1 ,obj2))]
       [else `(< ,obj1 ,obj2)])]
    [else
      `(< ,obj1 ,obj2)]))

(define (prim:> obj1 obj2)
  (syncase obj1
    [`(marker number ,x)
     (syncase obj2
       [`(marker number ,y) `(marker boolean ,(> x y))]
       [`(+ (marker number ,y) . ,z)
	(if (> y x)
	  `(marker boolean #f)
	  `(> ,obj1 ,obj2))]
       [else `(> ,obj1 ,obj2)])]
    [`(+ (marker number ,x) . ,z)
     (syncase obj2
       [`(marker number ,y)
	(if (> x y)
	  `(marker bookean #t)
	  `(> ,obj1 ,obj2))]
       [else `(> ,obj1 ,obj2)])]
    [else
      `(> ,obj1 ,obj2)]))

(define (prim:+ . args)
  (let loop ([args (reverse args)] [sum 0] [others '()])
    (syncase args
      [`()  (cond
	      [(null? others) `(marker number ,sum)]
	      [(zero? sum) (cons '+ others)]
	      [else `(+ (marker number ,sum) . ,others)])]
      [`((marker number ,n) . ,rest)
       (loop rest (+ sum n) others)]
      [`((+ (marker number ,n) . ,rest1) . ,rest2)
       (loop rest2 (+ sum n) (append rest1 others))]
      [`(,arg0 . ,rest) (loop rest sum (cons arg0 others))])))

(define (prim:list a b c)
  (prim:cons a (prim:cons b (prim:cons c '(marker null '())))))

(define (prim:cadr a)
  (prim:car (prim:cdr a)))

(define (prim:caddr a)
  (prim:car (prim:cdr (prim:cdr a))))

(define (prim:merge-paths . args)
  (let loop ([args args] [new-args '()])
    (syncase args
      [`() (cond
	     [(null? new-args) '(marker null '())]
	     [(null? (cdr new-args)) (car new-args)]
	     [else (cons 'merge-paths (reverse new-args))])]
      [`((marker null '()) . ,rest) (loop rest new-args)]
      [`(,arg0 . ,rest) (loop rest (cons arg0 new-args))])))


;;;
;;; environments
;;;

(define (make-initial-environment)
  `((car . ,prim:car)
    (cdr . ,prim:cdr)
    (cons . ,prim:cons)
    (null? . ,prim:null?)
    (< . ,prim:<)
    (> . ,prim:>)
    (+ . ,prim:+)
    (list . ,prim:list)
    (cadr . ,prim:cadr)
    (caddr . ,prim:caddr)
    (merge-paths . ,prim:merge-paths)
    (Tcar . (marker number ,Tcar))
    (Tcdr . (marker number ,Tcdr))
    (Tcons . (marker number ,Tcons))
    (Tnull? . (marker number ,Tnull?))
    (Teq? . (marker number ,Teq?))
    (T+ . (marker number ,T+))
    (T- . (marker number ,T-))
    (T* . (marker number ,T*))
    (T> . (marker number ,T>))
    (T< . (marker number ,T<))
    (T= . (marker number ,T=))
    (Tc . (marker number ,Tc))
    (Tvar . (marker number ,Tvar))
    (Tif . (marker number ,Tif))
    (Tlet . (marker number ,Tlet))
    (Tcall . (marker number ,Tcall))))

(define (env->value env var)
  (let ([x (assq var env)])
    (if x
      (cdr x)
      var)))

(define (extend-env env var val)
  `((,var . ,val) . ,env))

(define (extend-env* env vars vals)
  `(,@(map cons vars vals) . ,env))


;;;
;;; evaluator
;;;

(define (eval exp env k)
  (syncase exp
    [v (guard (symbol? v))
      (let ([val (env->value env v)])
	(if (simple-value? val) (k val '()) (k v (list v))))]
    [c (guard (atom? c)) (k (value->marked-value c) '())]
    [`(,'quote ,constant) (k (value->marked-value constant) '())]
    [`(,'quasiquote ,quasiconstant) (k `(marker ,'quasiquote ,exp) '())]
    [`(define ,proc ,body)
     (eval body env
       (lambda (exp vars-used)
	 (k `(define ,proc ,exp) vars-used)))]
    [`(lambda ,args ,body)
     (eval body env
       (lambda (exp vars-used)
	 (k `(lambda ,args ,exp) vars-used)))]
    [`(let ([,vars ,vals] ...) ,body)
     (map-eval vals env
       (lambda (vals vars-used)
	 (eval body (extend-env* env vars vals)
	   (lambda (body body-vars-used)
	     (let ([new-bindings (filter-unused-vars vars vals body-vars-used)])
	       (k
		 (if (null? new-bindings) body `(let ,new-bindings ,body))
		 (set:union (set:diff body-vars-used vars) vars-used)))))))]
    [`(if ,test-exp ,then-exp ,else-exp)
     (map-eval (cdr exp) env
       (lambda (exps vars-used)
	 (syncase exps
	   [`((marker boolean #f) ,then-exp ,else-exp) (k else-exp vars-used)]
	   [`((marker . ,ignored) ,then-exp ,else-exp) (k then-exp vars-used)]
	   [else (k (cons 'if exps) vars-used)])))]
    [`(,proc . ,args)
     (map-eval exp env
       (lambda (exps vars-used)
	 (if (procedure? (car exps))
	   (k (apply (car exps) (cdr exps)) vars-used)
	   (k exps vars-used))))]))


(define (map-eval ls env k)
  (let loop ([ls ls]
	     [exps '()]
	     [vars '()])
    (if (null? ls)
      (k (reverse exps) vars)
      (eval (car ls) env
	(lambda (exp0 vars0)
	  (loop (cdr ls) (cons exp0 exps) (set:union vars0 vars)))))))

;;;
;;; sets
;;;

(define (set:union set1 set2)
  (append set1 set2))

(define (set:diff set1 set2)
  (do ([set1 set1 (remove (car set2) set1)]
       [set2 set2 (cdr set2)])
    [(null? set2) set1]))

;;;
;;; utils
;;;

(define (filter-unused-vars vars vals used-vars)
  (let loop ([vars vars] [vals vals] [bindings '()])
    (if (null? vars)
      (reverse bindings)
      (loop (cdr vars) (cdr vals)
	(if (member (car vars) used-vars)
	  (cons (list (car vars) (car vals)) bindings)
	  bindings)))))


(define (value->marked-value val)
  (cond
    [(null? val) '(marker null '())]
    [(number? val) `(marker number ,val)]
    [(boolean? val) `(marker boolean ,val)]
    [(pair? val) `(marker pair ,(car val) ,(cdr val))]
    [else (error 'value->marked-value "unknown value: ~a" val)]))

(define (simple-value? val)
  (or (symbol? val)
      (and (pair? val) (eq? (car val) 'marker))
      (procedure? val)))

(define (marked->unmarked exp)
  (syncase exp
    [`(marker null '()) ''()]
    [`(marker number ,n) n]
    [`(marker boolean ,b) b]
    [`(marker pair ,the-car ,the-cdr)
     `(cons ,(marked->unmarked the-car) ,(marked->unmarked the-cdr))]
    [`(marker quote ,exp) exp]
    [`(marker quasiquote ,exp) exp]
    [`(marker . ,ignored) (error 'marker->unmarked "unknown mark: ~a" exp)]
    [x (guard (atom? x)) x]
    [else (map marked->unmarked exp)]))


(define (optimize x)
  (marked->unmarked
    (eval x (make-initial-environment)
      (lambda (exp vars-used)
	exp))))

optimize))


