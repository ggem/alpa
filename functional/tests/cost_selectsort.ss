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
    (define =_1 (make-proc2 =))
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


    (define cost_sort
      (lambda (x)
        (+ COST_COND (+ (+ COST_NULL COST_VARREF) (let ((x:1 (null?_1 x)))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (max
                                                          COST_CONSTANT
                                                          (let ((k (least x)))
                                                            (+ COST_BINDING
                                                               (+ (+ COST_FUNCALL
                                                                     (+ (cost_least
                                                                         x)
                                                                        COST_VARREF))
                                                                  (+ COST_CONS
                                                                     (+ COST_VARREF
                                                                        (+ COST_FUNCALL
                                                                           (+ (cost_sort
                                                                               (rest
                                                                                 x
                                                                                 k))
                                                                              (+ COST_FUNCALL
                                                                                 (+ (cost_rest
                                                                                     x
                                                                                     k)
                                                                                    (+ COST_VARREF
                                                                                       COST_VARREF)))))))))))
                                                        (if x:1
                                                            COST_CONSTANT
                                                            (let ((k
                                                              (least x)))
                                                              (+ COST_BINDING
                                                                 (+ (+ COST_FUNCALL
                                                                       (+ (cost_least
                                                                           x)
                                                                          COST_VARREF))
                                                                    (+ COST_CONS
                                                                       (+ COST_VARREF
                                                                          (+ COST_FUNCALL
                                                                             (+ (cost_sort
                                                                                 (rest
                                                                                   x
                                                                                   k))
                                                                                (+ COST_FUNCALL
                                                                                   (+ (cost_rest
                                                                                       x
                                                                                       k)
                                                                                      (+ COST_VARREF
                                                                                         COST_VARREF)))))))))))))))))

    (define sort
      (lambda (x)
        (let ((x:1 (null?_1 x)))
          (if (eq? x:1 'UNKNOWN) (lub_1 '() (let ((k (least x)))
                                              (cons k (sort (rest x k)))))
              (if x:1 '() (let ((k (least x)))
                            (cons k (sort (rest x k)))))))))

    (define cost_least
      (lambda (x)
        (+ COST_COND
           (+ (+ COST_NULL (+ COST_CDR COST_VARREF))
              (let ((x:1 (null?_1 (cdr_1 x))))
                (if (eq? x:1 'UNKNOWN)
                    (max (+ COST_CAR COST_VARREF) (let ((s (least (cdr_1 x))))
                                                    (+ COST_BINDING
                                                       (+ (+ COST_FUNCALL
                                                             (+ (cost_least
                                                                 (cdr x))
                                                                (+ COST_CDR
                                                                   COST_VARREF)))
                                                          (+ COST_COND
                                                             (+ (+ COST_BOOLEANOP
                                                                   (+ (+ COST_CAR
                                                                         COST_VARREF)
                                                                      COST_VARREF))
                                                                (let ((x:1
                                                                  (<_1
                                                                        (car_1
                                                                          x)
                                                                        s)))
                                                                  (if (eq? x:1
                                                                         'UNKNOWN)
                                                                      (max
                                                                        (+ COST_CAR
                                                                           COST_VARREF)
                                                                        COST_VARREF)
                                                                      (if x:1
                                                                          (+ COST_CAR
                                                                             COST_VARREF)
                                                                          COST_VARREF)))))))))
                    (if x:1
                        (+ COST_CAR COST_VARREF)
                        (let ((s (least (cdr_1 x))))
                          (+ COST_BINDING
                             (+ (+ COST_FUNCALL
                                   (+ (cost_least (cdr x))
                                      (+ COST_CDR COST_VARREF)))
                                (+ COST_COND
                                   (+ (+ COST_BOOLEANOP
                                         (+ (+ COST_CAR COST_VARREF)
                                            COST_VARREF))
                                      (let ((x:1 (<_1 (car_1 x) s)))
                                        (if (eq? x:1 'UNKNOWN)
                                            (max
                                              (+ COST_CAR COST_VARREF)
                                              COST_VARREF)
                                            (if x:1
                                                (+ COST_CAR COST_VARREF)
                                                COST_VARREF)))))))))))))))

    (define least
      (lambda (x)
        (let ((x:1 (null?_1 (cdr_1 x))))
          (if (eq? x:1 'UNKNOWN) (lub_1 (car_1 x) (let ((s (least (cdr_1 x))))
                                                    (let ((x:1
                                                      (<_1 (car_1 x) s)))
                                                      (if (eq? x:1 'UNKNOWN)
                                                          (lub_1 (car_1 x) s)
                                                          (if x:1
                                                              (car_1 x)
                                                              s)))))
              (if x:1 (car_1 x) (let ((s (least (cdr_1 x))))
                                  (let ((x:1 (<_1 (car_1 x) s)))
                                    (if (eq? x:1 'UNKNOWN)
                                        (lub_1 (car_1 x) s)
                                        (if x:1 (car_1 x) s)))))))))

    (define cost_rest
      (lambda (x k)
        (+ COST_COND
           (+ (+ COST_NULL (+ COST_CDR COST_VARREF))
              (let ((x:1 (null?_1 (cdr_1 x))))
                (if (eq? x:1 'UNKNOWN)
                    (max
                      COST_CONSTANT
                      (+ COST_COND
                         (+ (+ COST_BOOLEANOP
                               (+ COST_VARREF (+ COST_CAR COST_VARREF)))
                            (let ((x:1 (=_1 k (car_1 x))))
                              (if (eq? x:1 'UNKNOWN)
                                  (max
                                    (+ COST_CDR COST_VARREF)
                                    (+ COST_CONS
                                       (+ (+ COST_CAR COST_VARREF)
                                          (+ COST_FUNCALL
                                             (+ (cost_rest (cdr x) k)
                                                (+ (+ COST_CDR COST_VARREF)
                                                   COST_VARREF))))))
                                  (if x:1
                                      (+ COST_CDR COST_VARREF)
                                      (+ COST_CONS
                                         (+ (+ COST_CAR COST_VARREF)
                                            (+ COST_FUNCALL
                                               (+ (cost_rest (cdr x) k)
                                                  (+ (+ COST_CDR COST_VARREF)
                                                     COST_VARREF)))))))))))
                    (if x:1
                        COST_CONSTANT
                        (+ COST_COND
                           (+ (+ COST_BOOLEANOP
                                 (+ COST_VARREF (+ COST_CAR COST_VARREF)))
                              (let ((x:1 (=_1 k (car_1 x))))
                                (if (eq? x:1 'UNKNOWN)
                                    (max
                                      (+ COST_CDR COST_VARREF)
                                      (+ COST_CONS
                                         (+ (+ COST_CAR COST_VARREF)
                                            (+ COST_FUNCALL
                                               (+ (cost_rest (cdr x) k)
                                                  (+ (+ COST_CDR COST_VARREF)
                                                     COST_VARREF))))))
                                    (if x:1
                                        (+ COST_CDR COST_VARREF)
                                        (+ COST_CONS
                                           (+ (+ COST_CAR COST_VARREF)
                                              (+ COST_FUNCALL
                                                 (+ (cost_rest (cdr x) k)
                                                    (+ (+ COST_CDR COST_VARREF)
                                                       COST_VARREF)))))))))))))))))

    (define rest
      (lambda (x k)
        (let ((x:1 (null?_1 (cdr_1 x))))
          (if (eq? x:1 'UNKNOWN) (lub_1 '() (let ((x:1 (=_1 k (car_1 x))))
                                              (if (eq? x:1 'UNKNOWN)
                                                  (lub_1
                                                    (cdr_1 x)
                                                    (cons (car_1 x)
                                                       (rest (cdr_1 x) k)))
                                                  (if x:1
                                                      (cdr_1 x)
                                                      (cons (car_1 x)
                                                         (rest
                                                          (cdr_1 x)
                                                          k))))))
              (if x:1 '() (let ((x:1 (=_1 k (car_1 x))))
                            (if (eq? x:1 'UNKNOWN)
                                (lub_1
                                  (cdr_1 x)
                                  (cons (car_1 x) (rest (cdr_1 x) k)))
                                (if x:1
                                    (cdr_1 x)
                                    (cons (car_1 x) (rest (cdr_1 x) k))))))))))

    (lambda args
      (cost_sort (length_1 (car args))))))
