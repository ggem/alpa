;;
;; storage passing style
;;


;; kill-set! takes an expression and returns a set!-free expression, a
;; list of the free reference subexpressions and a list of the
;; side-effected variables in the expression.
(define kill-set!
  (lambda (exp)
    (case (car exp)
      [(quote)
       (values exp '() '())]
      [(prim)
       (values exp '() '())]
      [(varref)
       (values exp (list exp) '())]
      [(begin)
       (let ([exp0 (cadr exp)] [exp1 (caddr exp)])
         (let-values ([(exp0-exp exp0-refs exp0-avars) (kill-set! exp0)]
                      [(exp1-exp exp1-refs exp1-avars) (kill-set! exp1)])
           (values `(begin ,exp0-exp ,exp1-exp)
             (append exp0-refs exp1-refs)
             (union exp0-avars exp1-avars))))]
      [(if)
       (let-values ([(t-exp t-refs t-avars) (kill-set! (cadr exp))]
                    [(c-exp c-refs c-avars) (kill-set! (caddr exp))]
                    [(a-exp a-refs a-avars) (kill-set! (cadddr exp))])
         (values `(if ,t-exp ,c-exp ,a-exp)
           (append t-refs c-refs a-refs)
           (union t-avars (union c-avars a-avars))))]
      [(set!)
       (let-values ([(val refs avars) (kill-set! (caddr exp))])
         (values `((prim cell-set!) (varref ,(cadr exp)) ,val)
           refs
           (union (list (cadr exp)) avars)))]
      [(let)
       (let ([vars (map car (cadr exp))]
             [exps (map cadr (cadr exp))]
             [body (caddr exp)])
         (let-values ([(body-exp body-refs body-avars) (kill-set! body)]
                      [(exps-exp exps-refs exps-avars) (map-kill-set! exps)])
           (let-values ([(bound-refs free-refs)
                         (split (lambda (v) (memq (cadr v) vars)) body-refs)]
                        [(bound-avars free-avars)
                         (split (lambda (v) (memq v vars)) body-avars)])
             (if (null? bound-avars)
                 (values `(let ,(map list vars exps-exp) ,body-exp)
                   (append exps-refs free-refs)
                   (union free-avars exps-avars))
                 (let-values ([(assigned-brefs unassigned-brefs)
                               (split (lambda (v) (memq (cadr v) bound-avars))
                                 bound-refs)])
                   (for-each (lambda (ref)
                               (set-car! ref '(prim cell-ref))
                               (set-cdr! ref `((varref ,(cadr ref)))))
                     assigned-brefs)
                   (values
                     `(let ,(map list vars exps-exp)
                        (let ,(map list bound-avars
                                (map (lambda (v)
                                       `((prim make-cell) (varref ,v)))
                                  bound-avars))
                          ,body-exp))
                     (append exps-refs free-refs)
                     (union free-avars exps-avars)))))))]
      [else (map-kill-set! exp)])))



(define map-kill-set!
  (lambda (ls)
    (if (null? ls)
        (values '() '() '())
        (let-values ([(head-exp head-refs head-avars) (kill-set! (car ls))]
                     [(tail-exp tail-refs tail-avars) (map-kill-set! (cdr ls))])
          (values (cons head-exp tail-exp)
            (append head-refs tail-refs)
            (union head-avars tail-avars))))))


;;;
;;; sps
;;;

(define sps-exp
  (lambda (exp)
    (case (car exp)
      [(quote)  `(values ,exp (varref $store))]
      [(prim)	`(values ,exp (varref $store))]
      [(varref)	`(values ,exp (varref $store))]
      [(begin)
       (let ([exp0 (cadr exp)] [exp1 (caddr exp)])
         (let ([exp0-sps (sps-exp exp0)]
               [exp1-sps (sps-exp exp1)])
           `(let-values ([($ignored $store) ,exp0-sps])
              ,exp1-sps)))]
      [(if)
       (let ([t-sps (sps-exp (cadr exp))]
             [c-sps (sps-exp (caddr exp))]
             [a-sps (sps-exp (cadddr exp))]
             [var (make-new-var 'test)])
         `(let-values ([(,var $store) ,t-sps])
            (if (varref ,var) ,c-sps ,a-sps)))]
      [(let)
       (let ([vars (map car (cadr exp))]
             [exps (map cadr (cadr exp))]
             [body (caddr exp)])
         (map-sps-exp exps vars (sps-exp body)))]
      [else
        (let ([vars (make-new-vars 'var (length exp))])
          (map-sps-exp exp vars (map (lambda (v) `(varref ,v))
                                  (append vars '($store)))))])))


(define map-sps-exp
  (lambda (exps vars body)
    (if (null? exps)
        body
        `(let-values ([(,(car vars) $store),(sps-exp (car exps))])
           ,(map-sps-exp (cdr exps) (cdr vars) body)))))

(define sps
  (lambda (exp)
    (let ([new-exp (simplify-sps (sps-exp exp) '())])
      (case (value-returned new-exp)
        [(both) new-exp]
        [(value) `(values ,new-exp (varref $store))]
        [(store) `(values 'void ,new-exp)]))))


;;;
;;; takes an expression to simplify and returns a new simplified
;;; expression
;;;

(define simplify-sps
  (lambda (exp env)
    (letrec ([simp
               (lambda (exp)
                 (case (car exp)
                   [(quote prim) exp]
                   [(varref)  (let ([binding (assq (cadr exp) env)])
                                (if binding (cdr binding) exp))]
                   [(values) `(values . ,(map simp (cdr exp)))]
                   [(if) `(if . ,(map simp (cdr exp)))]
                   [(let-values)
                    (let ([binding (caadr exp)]
                          [body (caddr exp)])
                      (let ([vars (car binding)]
                            [vals (simp (cadr binding))])
                        (if (eq? (car vals) 'values)
                            (if (simple? (cadr vals))
                                ;; if it is a simple value then add it to
                                ;; the environment.
                                (simplify-sps body
                                  (cons (cons (car vars) (cadr vals)) env))
                                ;; otherwise use a let.
                                `(let ([,(car vars) ,(cadr vals)])
                                   ,(simp body)))
                            (case (value-returned vals)
                              [(value)
                               `(let ([,(car vars) ,vals])
                                   ,(simp body))]
                              [(store)
                               `(let ([$store ,vals])
                                   ,(simp body))]
                              [(both)
                               `(let-values ((,vars ,vals)) ,(simp body))]))))]
                   [else (map simp exp)]))])
      (simp exp))))

(define value-returned
  (lambda (exp)
    (case (car exp)
      ((quote) 'value)
      ((values varref) 'both)
      ((if let let-values) (value-returned (caddr exp)))
      (else
        (if (and (pair? (car exp)) (eq? (caar exp) 'prim))
            (let ([prim (assq (cadar exp) primitives)])
              (if prim
                  (cadr (cddddr prim))
                  (error "~a is not a primitive" (car exp))))
            'both)))))

(define simple?
  (lambda (exp)
    (memq (car exp) '(quote varref prim))))


;;;
;;; store ADT procedures
;;;

(define store:marker 'this-is-a-store)

(define store:new
  (lambda ()
    (list store:marker 0)))

(define store:heap-pointer cadr)
(define store:values cddr)

(define store:update
  (lambda (store location value)
    `(,store:marker ,(store:heap-pointer store) (,location . ,value)
       . ,(store:values store))))

(define store:allocate
  (letrec ([loop (lambda (vals location alist)
                   (if (null? vals)
                       alist
                       (loop (cdr vals) (+ 1 location)
                         `((,location . ,(car vals)) . ,alist))))])
    (lambda (store vals)
      (let ([size (length vals)]
            [heap-pointer (store:heap-pointer store)])
        (let ([new-heap-pointer (+ heap-pointer size)])
          `(,store:marker ,new-heap-pointer .
             ,(loop vals heap-pointer (store:values store))))))))

(define store:lookup
  (lambda (store location)
    (cdr (assq location (store:values store)))))

(define store?
  (lambda (st)
    (and (pair? st) (eq? (car st) store:marker))))


;;;
;;; utils
;;;

(define make-symbol
  (lambda (fmt . args)
    (string->symbol (apply format fmt args))))

(define make-new-var
  (let ([alist '()]
        [make-var (lambda (prefix count)
                    (make-symbol "$~a:~a" prefix count))])
    (lambda (prefix)
      (let ([count (assq prefix alist)])
        (if count
            (begin
              (set-cdr! count (+ (cdr count) 1))
              (make-var prefix (cdr count)))
            (begin
              (set! alist `((,prefix . 0) . ,alist))
              (make-var prefix 0)))))))

(define make-new-vars
  (lambda (prefix n)
    (let loop ([n n])
      (if (= n 0)
          '()
          (let ([var (make-new-var prefix)])
            (cons var (loop (- n 1))))))))

(define split
  (letrec ([split 
             (lambda (pred ls)
               (if (null? ls)
                   (values '() '())
                   (let-values ([(yes no) (split pred (cdr ls))])
                     (if (pred (car ls))
                         (values (cons (car ls) yes) no)
                         (values yes (cons (car ls) no))))))])
    split))

(define set-add
  (lambda (item set)
    (if (memq item set)
        set
        (cons item set))))

(define union
  (lambda (set1 set2)
    (let loop ([set1 set1])
      (if (null? set1)
          set2
          (set-add (car set1) (loop (cdr set1)))))))

(define setify
  (lambda (ls)
    (let loop ([ls ls])
      (if (null? ls)
          '()
          (set-add (car ls) (loop (cdr ls)))))))

(define make-cell box)
(define cell-ref unbox)
(define cell-set! set-box!)

(define list-set!
  (lambda (dst src)
    (set-car! dst (car src))
    (set-cdr! dst (cdr src))))


;;;
;;;
;;;
;;;



(define (test x) (simplify-sps (sps-exp (normalize x)) '()))