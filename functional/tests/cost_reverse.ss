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


    (define cost_reverse
      (lambda (ls)
        (+ COST_FUNCALL
           (+ (cost_reverse2 ls '()) (+ COST_VARREF COST_CONSTANT)))))

    (define reverse
      (lambda (ls)
        (reverse2 ls '())))

    (define cost_reverse2
      (lambda (ls answ)
        (+ COST_COND (+ (+ COST_NULL COST_VARREF) (let ((x:1 (null?_1 ls)))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (max
                                                          COST_VARREF
                                                          (+ COST_FUNCALL
                                                             (+ (cost_reverse2
                                                                 (cdr ls)
                                                                 (cons (car ls)
                                                                    answ))
                                                                (+ (+ COST_CDR
                                                                      COST_VARREF)
                                                                   (+ COST_CONS
                                                                      (+ (+ COST_CAR
                                                                            COST_VARREF)
                                                                         COST_VARREF))))))
                                                        (if x:1
                                                            COST_VARREF
                                                            (+ COST_FUNCALL
                                                               (+ (cost_reverse2
                                                                   (cdr ls)
                                                                   (cons (car ls)
                                                                      answ))
                                                                  (+ (+ COST_CDR
                                                                        COST_VARREF)
                                                                     (+ COST_CONS
                                                                        (+ (+ COST_CAR
                                                                              COST_VARREF)
                                                                           COST_VARREF))))))))))))

    (define reverse2
      (lambda (ls answ)
        (let ((x:1 (null?_1 ls)))
          (if (eq? x:1 'UNKNOWN)
              (lub_1 answ (reverse2 (cdr_1 ls) (cons (car_1 ls) answ)))
              (if x:1 answ (reverse2 (cdr_1 ls) (cons (car_1 ls) answ)))))))

    (lambda args
      (cost_reverse (length_1 (car args))))))
