;;;
;;;
;;;


;; (load "sps.ss")

(define alpa
  (lambda (program)
    (if (null? (cdr program))
        (list (alpa-body (cadar program)))
        (let ([definition (car program)])
          (let ([name (caadr definition)]
                [args (cdadr definition)]
                [body (caddr definition)])
            (let ([body-alpaed (simplify (T (sps (normalize body))))])
              (let ([body-cost  (simplify `(1st ,body-alpaed))]
                    [body-value (simplify `(2nd ,body-alpaed))])
              `((define (,name ,@args $store) ,body-value)
                (define (,(make-symbol "cost-~a" name) ,@args $store) ,body-cost)
                . ,(alpa (cdr program))))))))))


(define alpa-body
  (letrec ([process
             (lambda (exp arg-count k)
               (cond
                 [(eq? exp 'size)
                  (let ([var (make-symbol "size~a" arg-count)])
                    (k var (+ arg-count 1) `(,var) '() '()))]
                 [(atom? exp)
                  (k exp arg-count '() '() '())]
                 [(eq? (car exp) 'make-list-unknowns)
                  (let ([var  (make-symbol "size~a" arg-count)]
                        [list (make-symbol "list-size~a" arg-count)])
                    (k list (+ arg-count 1) `(,var)
                      `(((,list $store) (make-list-unknowns ,var $store))) '()))]
                 [(eq? (car exp) 'make-vector-unknowns)
                  (let ([var  (make-symbol "size~a" arg-count)]
                        [list (make-symbol "vector-size~a" arg-count)])
                    (k list (+ arg-count 1) `(,var) '()
                      `(((,list $store) (make-vector^ ,var 'unknown $store)))))]
                 [else
                   (process (car exp) arg-count
                     (lambda (car-exp arg-count args-car sbindings-car dbindings-car)
                       (process (cdr exp) arg-count
                         (lambda (cdr-exp arg-count args-cdr sbindings-cdr dbindings-cdr)
                           (k (cons car-exp cdr-exp)
                             arg-count
                             (append args-car args-cdr)
                             (append sbindings-car sbindings-cdr)
                             (append dbindings-car dbindings-cdr))))))]))])
    (lambda (body)
      (process body 0
        (lambda (exp arg-count args single-bindings double-bindings)
          `(lambda ,args
             (let ([$store (store:new)])
               (let ,single-bindings
                 (let-values ,double-bindings
                   (cost-vector->exp
                     ,(simplify `(1st ,(T (sps (normalize exp)))))))))))))))


(define normalize
  (let ()
    (define normalize
      (lambda (exp)
        (if (atom? exp)
            (if (symbol? exp)
                (if (primitive? exp)
                    `(prim ,exp)
                    `(varref ,exp))
                `(quote ,exp))
            (case (car exp)
              [(quote) exp]
              [(if)
               (if (null? (cdddr exp))
                   `(if . ,(map normalize (append (cdr exp) '('void))))
                   `(if . ,(map normalize (cdr exp))))]
              [(begin)
               (let ([exps (map normalize (cdr exp))])
                 (let loop ([exp0 (car exps)]
                            [exps (cdr exps)])
                   (if (null? exps)
                       exp0
                       `(begin ,exp0 ,(loop (car exps) (cdr exps))))))]
              [(set!)
               `(set! ,(cadr exp) ,(normalize (caddr exp)))]
              [(define)
               `(define ,(cadr exp) ,(normalize (caddr exp)))]
              [(let letrec)
               `(,(car exp) ,(map (lambda (x)
                                    (list (car x) (normalize (cadr x))))
                               (cadr exp))
                 ,(normalize `(begin . ,(cddr exp))))]
              [else
                (map normalize exp)]))))
    normalize))

(define T
  (let ()
    (define T                           ; make it a local definition
      (lambda (exp)
        (case (car exp)
          [(quote) `(pair cost_const ,exp)]
          [(prim)
           (primitive->pair (cadr exp))]
          [(varref)
           `(pair cost_varref ,(cadr exp))]
          [(begin)
           (error 'T "this begin here.  This shouldn't happen. ~s" exp)
           (let ([Texp0 (T (cadr exp))]
                 [Texp1 (T (caddr exp))])
             `(pair (c+ (1st ,Texp0) (1st ,Texp1)) (2nd ,Texp1)))]
          [(if)
           (let ([Ttest (T (cadr exp))]
                 [Tthen (T (caddr exp))]
                 [Telse (T (cadddr exp))]
                 [fresh (make-new-var 'fresh)]
                 [st1   (make-new-var 'store)]
                 [st2   (make-new-var 'store)]
                 [val1  (make-new-var 'val)]
                 [val2  (make-new-var 'val)])
             `(let ([,fresh (2nd ,Ttest)])
                (if (unknown? ,fresh)
                    (pair (c+ cost_if (1st ,Ttest)
                            (cmax (1st ,Tthen) (1st ,Telse)))
                      (let-values ([(,val1 ,st1) (2nd ,Tthen)])
                        (let-values ([(,val2 ,st2) (2nd ,Telse)])
                          (values (lub ,val1 ,val2) (lub ,st1 ,st2)))))
                    (if ,fresh
                        (pair (c+ cost_if (1st ,Ttest) (1st ,Tthen)) (2nd ,Tthen))
                        (pair (c+ cost_if (1st ,Ttest) (1st ,Telse))
                          (2nd ,Telse))))))]
          [(let)
           (let* ([bindings (cadr exp)]
                  [body (caddr exp)]
                  [Tbody (T body)]
                  [vars (map car bindings)]
                  [exps (map cadr bindings)]
                  [Texps (map T exps)]
                  [1stTexps (map (lambda (x) `(1st ,x)) Texps)]
                  [2ndTexps (map (lambda (x) `(2nd ,x)) Texps)]
                  [Tbindings (map list vars 2ndTexps)])
             `(let ,Tbindings
                (pair (c+ cost_let (1st ,Tbody) ,@1stTexps) (2nd ,Tbody))))]
          [(let-values)
           (let ([vars (caaadr exp)]
                 [Tbexp (T (cadr (caadr exp)))]
                 [Tbody (T (caddr exp))])
             `(let-values ((,vars ,Tbexp))
                (pair (c+ (1st ,Tbody) (1st ,Tbexp)) (2nd ,Tbody))))]
          [(values)
           (let ([Tval (T (cadr exp))]
                 [Tsto (T (caddr exp))])
             `(pair (c+ (1st ,Tval) (1st ,Tsto))
                (values (2nd ,Tval) (2nd ,Tsto))))]
          [else
            (let ([rator (cadar exp)]
                  [rands (cdr exp)])
              (let ([rator-cost (make-symbol "cost-~a" rator)]
                    [Trands (map T rands)])
                (let* ([1stTrands (map (lambda (x) `(1st ,x)) Trands)]
                       [2ndTrands (map (lambda (x) `(2nd ,x)) Trands)])
                  (if (primitive? rator)
                      `(pair (c+ ,(primitive->cost rator) ,@1stTrands)
                         (,(primitive->prim^ rator) ,@2ndTrands))
                      `(pair (c+ cost_funcall ,@1stTrands ;(1st ,Trator)
                               (,rator-cost ,@2ndTrands))
                         (,rator ,@2ndTrands))))))])))
    T))


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


;;;
;;; store-aware primitives
;;;

(define sa-cons
  (lambda (the-car the-cdr store)
    (let ([hp (store:heap-pointer store)])
      (values hp (store:allocate store (list the-car the-cdr))))))

(define sa-car
  (lambda (location store)
    (store:lookup store location)))

(define sa-cdr
  (lambda (location store)
    (store:lookup store (+ location 1))))

(define sa-set-car!
  (lambda (location value store)
    (store:update store location value)))

(define sa-set-cdr!
  (lambda (location value store)
    (store:update store (+ location 1) value)))

(define sa-make-vector
  (lambda (size init store)
    (let ([hp (store:heap-pointer store)])
      (let ([new-store (store:allocate store (cons size (make-list size init)))])
        (values hp new-store)))))

(define sa-vector
  (lambda args
    (let ([revargs (reverse args)])
      (let ([store (car revargs)]
            [args (reverse (cdr revargs))])
        (let ([size (length args)])
          (let ([hp (store:heap-pointer store)])
            (let ([new-store (store:allocate store (cons size args))])
              (values hp new-store))))))))

(define sa-vector-ref
  (lambda (location offset store)
    (store:lookup store (+ location offset 1))))

(define sa-vector-length
  (lambda (location store)
    (store:lookup store location)))

(define sa-vector-set!
  (lambda (location offset value store)
    (store:update store (+ location offset 1) value)))

(define make-sa-aware
  (lambda (proc arity)
    (case arity
      [(1) (lambda (arg1 store) (proc arg1))]
      [(2) (lambda (arg1 arg2 store) (proc arg1 arg2))]
      [else (error 'make-sa-aware "too many arguments: ~a ~a" proc arity)])))

(define sa-null? (make-sa-aware null? 1))
(define sa-eq? (make-sa-aware eq? 2))
(define sa-+ (make-sa-aware + 2))
(define sa-- (make-sa-aware - 2))
(define sa-* (make-sa-aware * 2))
(define sa-quotient (make-sa-aware quotient 2))
(define sa-> (make-sa-aware > 2))
(define sa-< (make-sa-aware < 2))
(define sa-= (make-sa-aware = 2))

;;;
;;;
;;;

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
      `(pair ,(caddr prim) ,(cadddr prim)))))

(define primitive->cost
  (lambda (name)
    (caddr (assq name primitives))))

(define primitive->prim^
  (lambda (name)
    (or (cadddr (assq name primitives))
        name)))

(define non-primitive-costs
  '(cost_const cost_varref cost_if cost_let cost_funcall))

(define constructors
  '(cons vector))

(define primitives
  (map (lambda (name+args)
         (let ([name (car name+args)]
               [retval (cadr name+args)]
               [args (cddr name+args)])
           (list name
             (top-level-value (make-symbol "sa-~a" name))
             (make-symbol "cost_~a" name)
             (make-symbol "~a^" name)
             args
             retval)))
    '((cons		both  . (#f #f))
      (car		value . (#t))
      (cdr		value . (#t))
      (set-car!		store . (#t #f))
      (set-cdr!		store . (#t #f))
      (make-vector	both  . (#t #f))
      (vector		both  . #f)
      (vector-ref	value . (#t #t))
      (vector-length	value . (#t))
      (vector-set!	store . (#t #t #f))
      (null?		value . (#t))
      (eq?		value . (#t #t))
      (+		value . (#t #t))
      (-		value . (#t #t))
      (*		value . (#t #t))
      (quotient		value . (#t #t))
      (>		value . (#t #t))
      (<		value . (#t #t))
      (=		value . (#t #t)))))

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
                     [let-values (bindings body)
                       `(let-values ((,(caar bindings) ,(simplify-pairs '2nd (cadar bindings)))) ,(simp body))]
		     [letrec (bindings body)
		       `(letrec ,bindings ,(simp body))]
		     [pair (cost value)
		       (if (eq? nth '1st) cost value)]
		     [else
		       `(,nth ,exp)])))])
      (simp exp))))


(define costs-names
  (append (map caddr primitives) non-primitive-costs))

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
    (define simp
      (lambda (exp)
        (cond
          [(assq exp costs) => cdr]	;;; get the cost vector if it exists
          [(atom? exp) exp]
          [else
            (record-case exp
              [quote (quoted-exp) exp]
              [if (test-exp then-exp else-exp)
                  `(if ,(simp test-exp)
                       ,(simp then-exp)
                       ,(simp else-exp))]
              [let (bindings body)
                `(let ,(map (lambda (b) (list (car b) (simp (cadr b))))
                         bindings)
                   ,(simp body))]
              [let-values (bindings body)
                `(let-values ((,(caar bindings) ,(simp (cadar bindings))))
                   ,(simp body))]
              [letrec (bindings body)
                `(letrec ,(map (lambda (b) (list (car b) (simp (cadr b))))
                            bindings)
                   ,(simp body))]
              [lambda (formals body)
                `(lambda ,formals ,(simp body))]
              [pair (cost-function value-function)
                `(pair ,(simp cost-function) ,(simp value-function))]
              [1st (possible-pair)
                (let ([pp (simp possible-pair)])
                  (simplify-pairs '1st pp))]
              [2nd (possible-pair)
                (let ([pp (simp possible-pair)])
                  (simplify-pairs '2nd pp))]
              [cmax (c1 c2)
                (let ([c1 (simp c1)]
                      [c2 (simp c2)])
                  (if (and (vector? c1) (vector? c2))
                      (cmax c1 c2)
                      `(cmax ,c1 ,c2)))]
              [c+ args
                (simplify-add (map simp args))]
              [else
                (map simp exp)])])))
    simp))

;;;
;;;


(define scheme-wrap
  (let* ([unknown-check
	   (lambda (var)
	     `(eq? ,var 'unknown))]
	 [make-unknown-aware
           (let ([args-names-v '#(() (x) (x y) (x y z))])
             (lambda (prim)
               (let ([name (car prim)]
                     [prim^ (cadddr prim)]
                     [args-checks (car (cddddr prim))])
                 (let ([sa-name (make-symbol "sa-~a" name)])
                   (if (or (not args-checks) (andmap not args-checks))
                       `(define ,prim^ ,sa-name)
                       (let ([all-args (vector-ref args-names-v (length args-checks))])
                         (do ([to-check args-checks (cdr to-check)]
                              [args all-args (cdr args)]
                              [check '() (if (car to-check) (cons (unknown-check (car args)) check) check)])
                             [(null? to-check)
                              `(define (,prim^ ,@all-args $store)
                                 (if (or . ,check)
                                     'unknown
                                     (,sa-name ,@all-args $store)))])))))))])
    (lambda (exps)
      `(define ,(string->symbol (format "cost-~a" (caadar exps)))
	 (let ()
           ,@(map make-unknown-aware primitives)

	   ,@exps)))))



(define (lub x y)
  (cond
    [(and (store? x) (store? y)) (lub-store x y)]
    [(equal? x y) x]
    [(atom? x) 'unknown]
    [(atom? y) 'unknown]
    [else (cons (lub (car x) (car y)) (lub (cdr x) (cdr y)))]))
           
(define (lub-store st1 st2)
  (define (store->vector st size)
    (let ([vec (make-vector size 'unknown)])
      (do ([vals (reverse (store:values st)) (cdr vals)])
          [(null? vals) vec]
        (vector-set! vec (caar vals) (cdar vals)))))
  (let ([size (max (store:heap-pointer st1) (store:heap-pointer st2))])
    (let ([v1 (store->vector st1 size)]
          [v2 (store->vector st2 size)])
      (do ([i (- size 1) (- i 1)]
           [vals '() `((,i . ,(lub (vector-ref v1 i) (vector-ref v2 i)))
                       . ,vals)])
          [(< i 0) `(,store:marker ,size . ,vals)]))))

(define (make-list-unknowns n) (make-list n 'unknown))
(define (make-vector-unknowns n) (make-vector n 'unknown))
(define (unknown? x) (eq? x 'unknown))
(define (varref x) x)
(define (prim x) x)

;;;
;;;
;;;

(define alpa-file
  (let ([read-file (lambda (filename)
		     (let ([ip (open-input-file filename)])
		       (do ([ls '() (cons x ls)]
			    [x (read ip) (read ip)])
			 [(eof-object? x)
                          (close-input-port ip)
                          (reverse ls)])))])
    (lambda (filename)
      (alpa (read-file filename)))))

(define alpa-process-file
  (lambda (ifn ofn)
    (with-output-to-file ofn
      (lambda ()
        (parameterize ([pretty-line-length 132]
                       [print-vector-length #f])
          (pretty-print
            (scheme-wrap
              (alpa-file ifn)))))
      'replace)))


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

(define toggle-debug
  (let ([make-debugger
          (lambda (name proc)
            (lambda (exp)
              (parameterize ([print-level 3] [print-length 3])
                (pretty-print `(,name ,exp))
                (let ([answ (proc exp)])
                  (pretty-print answ)
                  answ))))])
    (let ([normalize-original normalize]
          [sps-original sps]
          [T-original T]
          [simplify-original simplify]
          [normalize-new (make-debugger 'normalize normalize)]
          [sps-new (make-debugger 'sps sps)]
          [T-new (make-debugger 'T T)]
          [simplify-new (make-debugger 'simplify simplify)]
          [debugging? #f])
      (lambda ()
        (if debugging?
            (begin
              (set! normalize normalize-original)
              (set! sps sps-original)
              (set! T T-original)
              (set! simplify simplify-original)
              (set! debugging? #f))
            (begin
              (set! normalize normalize-new)
              (set! sps sps-new)
              (set! T T-new)
              (set! simplify simplify-new)
              (set! debugging? #t)))
        (printf "Debugging is ~a.~%" debugging?)))))

;;;
;;;
;;;

