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
    (define null?_1 (make-proc1 null?))
    (define lub_1
      (lambda (x y)
        (cond
          [(and (eq? x y) (null? x)) x]
          [(or (eq? x 'unknown) (eq? y 'unknown) (atom? x) (atom? y))
           'unknown]
          [else
           (cons (lub_1 (car x) (car y)) (lub_1 (cdr x) (cdr y)))])))
    (define length_1
      (lambda (n)
        (if (= n 0) '() (cons 'unknown (length_1 (- n 1))))))
    (define cost_sort
      (lambda (lst)
        (+ cost_cond
           (+ (+ cost_null cost_varref)
              (let ([x:1 (null?_1 lst)])
                (if (eq? x:1 'unknown)
                    (max cost_constant
                         (+ cost_funcall
                            (+ (cost_insert (car lst) (sort (cdr lst)))
                               (+ (+ cost_car cost_varref)
                                  (+ cost_funcall
                                     (+ (cost_sort (cdr lst))
                                        (+ cost_cdr cost_varref)))))))
                    (if x:1
                        cost_constant
                        (+ cost_funcall
                           (+ (cost_insert (car lst) (sort (cdr lst)))
                              (+ (+ cost_car cost_varref)
                                 (+ cost_funcall
                                    (+ (cost_sort (cdr lst))
                                       (+ cost_cdr cost_varref)))))))))))))
    (define sort
      (lambda (lst)
        (let ([x:1 (null?_1 lst)])
          (if (eq? x:1 'unknown)
              (lub_1 '() (insert (car_1 lst) (sort (cdr_1 lst))))
              (if x:1 '() (insert (car_1 lst) (sort (cdr_1 lst))))))))
    (define cost_insert
      (lambda (a lst)
        (+ cost_cond
           (+ (+ cost_null cost_varref)
              (let ([x:1 (null?_1 lst)])
                (if (eq? x:1 'unknown)
                    (max (+ cost_cons (+ cost_varref cost_constant))
                         (+ cost_cond
                            (+ (+ cost_booleanop
                                  (+ cost_varref (+ cost_car cost_varref)))
                               (let ([x:1 (<_1 a (car_1 lst))])
                                 (if (eq? x:1 'unknown)
                                     (max (+ cost_cons
                                             (+ cost_varref cost_varref))
                                          (+ cost_cons
                                             (+ (+ cost_car cost_varref)
                                                (+ cost_funcall
                                                   (+ (cost_insert
                                                        a
                                                        (cdr lst))
                                                      (+ cost_varref
                                                         (+ cost_cdr
                                                            cost_varref)))))))
                                     (if x:1
                                         (+ cost_cons
                                            (+ cost_varref cost_varref))
                                         (+ cost_cons
                                            (+ (+ cost_car cost_varref)
                                               (+ cost_funcall
                                                  (+ (cost_insert
                                                       a
                                                       (cdr lst))
                                                     (+ cost_varref
                                                        (+ cost_cdr
                                                           cost_varref))))))))))))
                    (if x:1
                        (+ cost_cons (+ cost_varref cost_constant))
                        (+ cost_cond
                           (+ (+ cost_booleanop
                                 (+ cost_varref (+ cost_car cost_varref)))
                              (let ([x:1 (<_1 a (car_1 lst))])
                                (if (eq? x:1 'unknown)
                                    (max (+ cost_cons
                                            (+ cost_varref cost_varref))
                                         (+ cost_cons
                                            (+ (+ cost_car cost_varref)
                                               (+ cost_funcall
                                                  (+ (cost_insert
                                                       a
                                                       (cdr lst))
                                                     (+ cost_varref
                                                        (+ cost_cdr
                                                           cost_varref)))))))
                                    (if x:1
                                        (+ cost_cons
                                           (+ cost_varref cost_varref))
                                        (+ cost_cons
                                           (+ (+ cost_car cost_varref)
                                              (+ cost_funcall
                                                 (+ (cost_insert
                                                      a
                                                      (cdr lst))
                                                    (+ cost_varref
                                                       (+ cost_cdr
                                                          cost_varref))))))))))))))))))
    (define insert
      (lambda (a lst)
        (let ([x:1 (null?_1 lst)])
          (if (eq? x:1 'unknown)
              (lub_1
                (cons a '())
                (let ([x:1 (<_1 a (car_1 lst))])
                  (if (eq? x:1 'unknown)
                      (lub_1
                        (cons a lst)
                        (cons (car_1 lst) (insert a (cdr_1 lst))))
                      (if x:1
                          (cons a lst)
                          (cons (car_1 lst) (insert a (cdr_1 lst)))))))
              (if x:1
                  (cons a '())
                  (let ([x:1 (<_1 a (car_1 lst))])
                    (if (eq? x:1 'unknown)
                        (lub_1
                          (cons a lst)
                          (cons (car_1 lst) (insert a (cdr_1 lst))))
                        (if x:1
                            (cons a lst)
                            (cons (car_1 lst)
                                  (insert a (cdr_1 lst)))))))))))
    (lambda args (cost_sort (length_1 (car args))))))

