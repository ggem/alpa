(define costbound
  (let ([cost_addition 1]
        [cost_binding 1]
        [cost_booleanop 1]
        [cost_car 1]
        [cost_cdr 1]
        [cost_cond 1]
        [cost_cons 1]
        [cost_constant 1]
        [cost_division 1]
        [cost_funcall 1]
        [cost_multiplication 1]
        [cost_negation 1]
        [cost_null 1]
        [cost_varref 1]
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
        (length_1 (car (cdr args)))))))

