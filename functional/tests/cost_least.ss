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
    (define <=_1 (make-proc2 <=))
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
                                                                  (<=_1
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
                                      (let ((x:1 (<=_1 (car_1 x) s)))
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
                                                      (<=_1 (car_1 x) s)))
                                                      (if (eq? x:1 'UNKNOWN)
                                                          (lub_1 (car_1 x) s)
                                                          (if x:1
                                                              (car_1 x)
                                                              s)))))
              (if x:1 (car_1 x) (let ((s (least (cdr_1 x))))
                                  (let ((x:1 (<=_1 (car_1 x) s)))
                                    (if (eq? x:1 'UNKNOWN)
                                        (lub_1 (car_1 x) s)
                                        (if x:1 (car_1 x) s)))))))))

    (lambda args
      (cost_least (length_1 (car args))))))
