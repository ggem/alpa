(define costbound
  (let ([COST_ADDITION 1]
        [COST_BINDING 1]
        [COST_BOOLEANOP 1]
        [COST_CAR 1]
        [COST_CDR 1]
        [COST_COND 1]
        [COST_CONS 1]
        [COST_CONSTANT 1]
        [COST_DIVISION 1]
        [COST_FUNCALL 1]
        [COST_MULTIPLICATION 1]
        [COST_NEGATION 1]
        [COST_NULL 1]
        [COST_VARREF 1]
        [make-proc1
          (lambda (proc)
            (lambda (arg)
              (if (eq? arg (quote UNKNOWN))
                  (quote UNKNOWN)
                  (proc arg))))]
        [make-proc2
          (lambda (proc)
            (lambda (arg1 arg2)
              (cond
                [(eq? arg1 (quote UNKNOWN)) (quote UNKNOWN)]
                [(eq? arg2 (quote UNKNOWN)) (quote UNKNOWN)]
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
          [(atom? x) (quote UNKNOWN)]
          [else (cons (lub_1 (car x) (car y))
                  (lub_1 (cdr x) (cdr y)))])))
    (define length_1
      (lambda (n)
        (if (= n 0)
          '()
          (cons (quote UNKNOWN) (length_1 (- n 1))))))


    (define cost_union
      (lambda (l1 l2)
        (+ COST_COND (+ (+ COST_NULL COST_VARREF) (let ((x:1 (null?_1 l1)))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (max
                                                          COST_CONSTANT
                                                          (let ((rr
                                                            (union
                                                                  (cdr_1 l1)
                                                                  l2)))
                                                            (+ COST_BINDING
                                                               (+ (+ COST_FUNCALL
                                                                     (+ (cost_union
                                                                         (cdr l1)
                                                                         l2)
                                                                        (+ (+ COST_CDR
                                                                              COST_VARREF)
                                                                           COST_VARREF)))
                                                                  (+ COST_COND
                                                                     (+ (+ COST_FUNCALL
                                                                           (+ (cost_member?
                                                                               (car l1)
                                                                               l2)
                                                                              (+ (+ COST_CAR
                                                                                    COST_VARREF)
                                                                                 COST_VARREF)))
                                                                        (let ((x:1
                                                                          (member?
                                                                                (car_1
                                                                                  l1)
                                                                                l2)))
                                                                          (if (eq? x:1
                                                                                 'UNKNOWN)
                                                                              (max
                                                                                COST_VARREF
                                                                                (+ COST_CONS
                                                                                   (+ (+ COST_CAR
                                                                                         COST_VARREF)
                                                                                      COST_VARREF)))
                                                                              (if x:1
                                                                                  COST_VARREF
                                                                                  (+ COST_CONS
                                                                                     (+ (+ COST_CAR
                                                                                           COST_VARREF)
                                                                                        COST_VARREF)))))))))))
                                                        (if x:1
                                                            COST_CONSTANT
                                                            (let ((rr
                                                              (union
                                                                    (cdr_1 l1)
                                                                    l2)))
                                                              (+ COST_BINDING
                                                                 (+ (+ COST_FUNCALL
                                                                       (+ (cost_union
                                                                           (cdr l1)
                                                                           l2)
                                                                          (+ (+ COST_CDR
                                                                                COST_VARREF)
                                                                             COST_VARREF)))
                                                                    (+ COST_COND
                                                                       (+ (+ COST_FUNCALL
                                                                             (+ (cost_member?
                                                                                 (car l1)
                                                                                 l2)
                                                                                (+ (+ COST_CAR
                                                                                      COST_VARREF)
                                                                                   COST_VARREF)))
                                                                          (let ((x:1
                                                                            (member?
                                                                                  (car_1
                                                                                    l1)
                                                                                  l2)))
                                                                            (if (eq? x:1
                                                                                   'UNKNOWN)
                                                                                (max
                                                                                  COST_VARREF
                                                                                  (+ COST_CONS
                                                                                     (+ (+ COST_CAR
                                                                                           COST_VARREF)
                                                                                        COST_VARREF)))
                                                                                (if x:1
                                                                                    COST_VARREF
                                                                                    (+ COST_CONS
                                                                                       (+ (+ COST_CAR
                                                                                             COST_VARREF)
                                                                                          COST_VARREF)))))))))))))))))

    (define union
      (lambda (l1 l2)
        (let ((x:1 (null?_1 l1)))
          (if (eq? x:1 'UNKNOWN) (lub_1 '() (let ((rr (union (cdr_1 l1) l2)))
                                              (let ((x:1
                                                (member? (car_1 l1) l2)))
                                                (if (eq? x:1 'UNKNOWN)
                                                    (lub_1
                                                      rr
                                                      (cons (car_1 l1) rr))
                                                    (if x:1
                                                        rr
                                                        (cons (car_1 l1)
                                                           rr))))))
              (if x:1 '() (let ((rr (union (cdr_1 l1) l2)))
                            (let ((x:1 (member? (car_1 l1) l2)))
                              (if (eq? x:1 'UNKNOWN)
                                  (lub_1 rr (cons (car_1 l1) rr))
                                  (if x:1 rr (cons (car_1 l1) rr))))))))))

    (define cost_member?
      (lambda (a l)
        (+ COST_COND (+ (+ COST_NULL COST_VARREF) (let ((x:1 (null?_1 l)))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (max
                                                          COST_CONSTANT
                                                          (+ COST_COND
                                                             (+ (+ COST_BOOLEANOP
                                                                   (+ (+ COST_CAR
                                                                         COST_VARREF)
                                                                      COST_VARREF))
                                                                (let ((x:1
                                                                  (eq?_1
                                                                        (car_1
                                                                          l)
                                                                        a)))
                                                                  (if (eq? x:1
                                                                         'UNKNOWN)
                                                                      (max
                                                                        COST_CONSTANT
                                                                        (+ COST_FUNCALL
                                                                           (+ (cost_member?
                                                                               a
                                                                               (cdr l))
                                                                              (+ COST_VARREF
                                                                                 (+ COST_CDR
                                                                                    COST_VARREF)))))
                                                                      (if x:1
                                                                          COST_CONSTANT
                                                                          (+ COST_FUNCALL
                                                                             (+ (cost_member?
                                                                                 a
                                                                                 (cdr l))
                                                                                (+ COST_VARREF
                                                                                   (+ COST_CDR
                                                                                      COST_VARREF))))))))))
                                                        (if x:1
                                                            COST_CONSTANT
                                                            (+ COST_COND
                                                               (+ (+ COST_BOOLEANOP
                                                                     (+ (+ COST_CAR
                                                                           COST_VARREF)
                                                                        COST_VARREF))
                                                                  (let ((x:1
                                                                    (eq?_1
                                                                          (car_1
                                                                            l)
                                                                          a)))
                                                                    (if (eq? x:1
                                                                           'UNKNOWN)
                                                                        (max
                                                                          COST_CONSTANT
                                                                          (+ COST_FUNCALL
                                                                             (+ (cost_member?
                                                                                 a
                                                                                 (cdr l))
                                                                                (+ COST_VARREF
                                                                                   (+ COST_CDR
                                                                                      COST_VARREF)))))
                                                                        (if x:1
                                                                            COST_CONSTANT
                                                                            (+ COST_FUNCALL
                                                                               (+ (cost_member?
                                                                                   a
                                                                                   (cdr l))
                                                                                  (+ COST_VARREF
                                                                                     (+ COST_CDR
                                                                                        COST_VARREF))))))))))))))))

    (define member?
      (lambda (a l)
        (let ((x:1 (null?_1 l)))
          (if (eq? x:1 'UNKNOWN) (lub_1 #f (let ((x:1 (eq?_1 (car_1 l) a)))
                                             (if (eq? x:1 'UNKNOWN)
                                                 (lub_1
                                                   #t
                                                   (member? a (cdr_1 l)))
                                                 (if x:1
                                                     #t
                                                     (member? a (cdr_1 l))))))
              (if x:1 #f (let ((x:1 (eq?_1 (car_1 l) a)))
                           (if (eq? x:1 'UNKNOWN)
                               (lub_1 #t (member? a (cdr_1 l)))
                               (if x:1 #t (member? a (cdr_1 l))))))))))

    (lambda args
      (cost_union (length_1 (car args)) (length_1 (car (cdr args)))))))
