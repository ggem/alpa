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


    (define cost_mergesort
      (lambda (x)
        (+ COST_COND (+ (+ COST_NULL COST_VARREF) (let ((x:1 (null?_1 x)))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (max
                                                          COST_CONSTANT
                                                          (+ COST_COND
                                                             (+ (+ COST_NULL
                                                                   (+ COST_CDR
                                                                      COST_VARREF))
                                                                (let ((x:1
                                                                  (null?_1
                                                                        (cdr_1
                                                                          x))))
                                                                  (if (eq? x:1
                                                                         'UNKNOWN)
                                                                      (max
                                                                        (+ COST_CONS
                                                                           (+ (+ COST_CAR
                                                                                 COST_VARREF)
                                                                              COST_CONSTANT))
                                                                        (+ COST_FUNCALL
                                                                           (+ (cost_merge
                                                                               (mergesort
                                                                                 (odd
                                                                                   x))
                                                                               (mergesort
                                                                                 (even
                                                                                   x)))
                                                                              (+ (+ COST_FUNCALL
                                                                                    (+ (cost_mergesort
                                                                                        (odd
                                                                                          x))
                                                                                       (+ COST_FUNCALL
                                                                                          (+ (cost_odd
                                                                                              x)
                                                                                             COST_VARREF))))
                                                                                 (+ COST_FUNCALL
                                                                                    (+ (cost_mergesort
                                                                                        (even
                                                                                          x))
                                                                                       (+ COST_FUNCALL
                                                                                          (+ (cost_even
                                                                                              x)
                                                                                             COST_VARREF))))))))
                                                                      (if x:1
                                                                          (+ COST_CONS
                                                                             (+ (+ COST_CAR
                                                                                   COST_VARREF)
                                                                                COST_CONSTANT))
                                                                          (+ COST_FUNCALL
                                                                             (+ (cost_merge
                                                                                 (mergesort
                                                                                   (odd
                                                                                     x))
                                                                                 (mergesort
                                                                                   (even
                                                                                     x)))
                                                                                (+ (+ COST_FUNCALL
                                                                                      (+ (cost_mergesort
                                                                                          (odd
                                                                                            x))
                                                                                         (+ COST_FUNCALL
                                                                                            (+ (cost_odd
                                                                                                x)
                                                                                               COST_VARREF))))
                                                                                   (+ COST_FUNCALL
                                                                                      (+ (cost_mergesort
                                                                                          (even
                                                                                            x))
                                                                                         (+ COST_FUNCALL
                                                                                            (+ (cost_even
                                                                                                x)
                                                                                               COST_VARREF)))))))))))))
                                                        (if x:1
                                                            COST_CONSTANT
                                                            (+ COST_COND
                                                               (+ (+ COST_NULL
                                                                     (+ COST_CDR
                                                                        COST_VARREF))
                                                                  (let ((x:1
                                                                    (null?_1
                                                                          (cdr_1
                                                                            x))))
                                                                    (if (eq? x:1
                                                                           'UNKNOWN)
                                                                        (max
                                                                          (+ COST_CONS
                                                                             (+ (+ COST_CAR
                                                                                   COST_VARREF)
                                                                                COST_CONSTANT))
                                                                          (+ COST_FUNCALL
                                                                             (+ (cost_merge
                                                                                 (mergesort
                                                                                   (odd
                                                                                     x))
                                                                                 (mergesort
                                                                                   (even
                                                                                     x)))
                                                                                (+ (+ COST_FUNCALL
                                                                                      (+ (cost_mergesort
                                                                                          (odd
                                                                                            x))
                                                                                         (+ COST_FUNCALL
                                                                                            (+ (cost_odd
                                                                                                x)
                                                                                               COST_VARREF))))
                                                                                   (+ COST_FUNCALL
                                                                                      (+ (cost_mergesort
                                                                                          (even
                                                                                            x))
                                                                                         (+ COST_FUNCALL
                                                                                            (+ (cost_even
                                                                                                x)
                                                                                               COST_VARREF))))))))
                                                                        (if x:1
                                                                            (+ COST_CONS
                                                                               (+ (+ COST_CAR
                                                                                     COST_VARREF)
                                                                                  COST_CONSTANT))
                                                                            (+ COST_FUNCALL
                                                                               (+ (cost_merge
                                                                                   (mergesort
                                                                                     (odd
                                                                                       x))
                                                                                   (mergesort
                                                                                     (even
                                                                                       x)))
                                                                                  (+ (+ COST_FUNCALL
                                                                                        (+ (cost_mergesort
                                                                                            (odd
                                                                                              x))
                                                                                           (+ COST_FUNCALL
                                                                                              (+ (cost_odd
                                                                                                  x)
                                                                                                 COST_VARREF))))
                                                                                     (+ COST_FUNCALL
                                                                                        (+ (cost_mergesort
                                                                                            (even
                                                                                              x))
                                                                                           (+ COST_FUNCALL
                                                                                              (+ (cost_even
                                                                                                  x)
                                                                                                 COST_VARREF)))))))))))))))))))

    (define mergesort
      (lambda (x)
        (let ((x:1 (null?_1 x)))
          (if (eq? x:1 'UNKNOWN) (lub_1 '() (let ((x:1 (null?_1 (cdr_1 x))))
                                              (if (eq? x:1 'UNKNOWN)
                                                  (lub_1
                                                    (cons (car_1 x) '())
                                                    (merge
                                                      (mergesort (odd x))
                                                      (mergesort (even x))))
                                                  (if x:1
                                                      (cons (car_1 x) '())
                                                      (merge
                                                        (mergesort (odd x))
                                                        (mergesort
                                                          (even x)))))))
              (if x:1 '() (let ((x:1 (null?_1 (cdr_1 x))))
                            (if (eq? x:1 'UNKNOWN)
                                (lub_1
                                  (cons (car_1 x) '())
                                  (merge
                                    (mergesort (odd x))
                                    (mergesort (even x))))
                                (if x:1
                                    (cons (car_1 x) '())
                                    (merge
                                      (mergesort (odd x))
                                      (mergesort (even x)))))))))))

    (define cost_odd
      (lambda (x)
        (+ COST_COND (+ (+ COST_NULL COST_VARREF) (let ((x:1 (null?_1 x)))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (max
                                                          COST_CONSTANT
                                                          (+ COST_CONS
                                                             (+ (+ COST_CAR
                                                                   COST_VARREF)
                                                                (+ COST_FUNCALL
                                                                   (+ (cost_even
                                                                       (cdr x))
                                                                      (+ COST_CDR
                                                                         COST_VARREF))))))
                                                        (if x:1
                                                            COST_CONSTANT
                                                            (+ COST_CONS
                                                               (+ (+ COST_CAR
                                                                     COST_VARREF)
                                                                  (+ COST_FUNCALL
                                                                     (+ (cost_even
                                                                         (cdr x))
                                                                        (+ COST_CDR
                                                                           COST_VARREF))))))))))))

    (define odd
      (lambda (x)
        (let ((x:1 (null?_1 x)))
          (if (eq? x:1 'UNKNOWN)
              (lub_1 '() (cons (car_1 x) (even (cdr_1 x))))
              (if x:1 '() (cons (car_1 x) (even (cdr_1 x))))))))

    (define cost_even
      (lambda (x)
        (+ COST_COND (+ (+ COST_NULL COST_VARREF) (let ((x:1 (null?_1 x)))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (max
                                                          COST_CONSTANT
                                                          (+ COST_FUNCALL
                                                             (+ (cost_odd
                                                                 (cdr x))
                                                                (+ COST_CDR
                                                                   COST_VARREF))))
                                                        (if x:1
                                                            COST_CONSTANT
                                                            (+ COST_FUNCALL
                                                               (+ (cost_odd
                                                                   (cdr x))
                                                                  (+ COST_CDR
                                                                     COST_VARREF))))))))))

    (define even
      (lambda (x)
        (let ((x:1 (null?_1 x)))
          (if (eq? x:1 'UNKNOWN)
              (lub_1 '() (odd (cdr_1 x)))
              (if x:1 '() (odd (cdr_1 x)))))))

    (define cost_merge
      (lambda (x y)
        (+ COST_COND (+ (+ COST_NULL COST_VARREF) (let ((x:1 (null?_1 x)))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (max
                                                          COST_VARREF
                                                          (+ COST_COND
                                                             (+ (+ COST_NULL
                                                                   COST_VARREF)
                                                                (let ((x:1
                                                                  (null?_1 y)))
                                                                  (if (eq? x:1
                                                                         'UNKNOWN)
                                                                      (max
                                                                        COST_VARREF
                                                                        (+ COST_COND
                                                                           (+ (+ COST_BOOLEANOP
                                                                                 (+ (+ COST_CAR
                                                                                       COST_VARREF)
                                                                                    (+ COST_CAR
                                                                                       COST_VARREF)))
                                                                              (let ((x:1
                                                                                (<_1
                                                                                      (car_1
                                                                                        x)
                                                                                      (car_1
                                                                                        y))))
                                                                                (if (eq? x:1
                                                                                       'UNKNOWN)
                                                                                    (max
                                                                                      (+ COST_CONS
                                                                                         (+ (+ COST_CAR
                                                                                               COST_VARREF)
                                                                                            (+ COST_FUNCALL
                                                                                               (+ (cost_merge
                                                                                                   (cdr x)
                                                                                                   y)
                                                                                                  (+ (+ COST_CDR
                                                                                                        COST_VARREF)
                                                                                                     COST_VARREF)))))
                                                                                      (+ COST_CONS
                                                                                         (+ (+ COST_CAR
                                                                                               COST_VARREF)
                                                                                            (+ COST_FUNCALL
                                                                                               (+ (cost_merge
                                                                                                   x
                                                                                                   (cdr y))
                                                                                                  (+ COST_VARREF
                                                                                                     (+ COST_CDR
                                                                                                        COST_VARREF)))))))
                                                                                    (if x:1
                                                                                        (+ COST_CONS
                                                                                           (+ (+ COST_CAR
                                                                                                 COST_VARREF)
                                                                                              (+ COST_FUNCALL
                                                                                                 (+ (cost_merge
                                                                                                     (cdr x)
                                                                                                     y)
                                                                                                    (+ (+ COST_CDR
                                                                                                          COST_VARREF)
                                                                                                       COST_VARREF)))))
                                                                                        (+ COST_CONS
                                                                                           (+ (+ COST_CAR
                                                                                                 COST_VARREF)
                                                                                              (+ COST_FUNCALL
                                                                                                 (+ (cost_merge
                                                                                                     x
                                                                                                     (cdr y))
                                                                                                    (+ COST_VARREF
                                                                                                       (+ COST_CDR
                                                                                                          COST_VARREF))))))))))))
                                                                      (if x:1
                                                                          COST_VARREF
                                                                          (+ COST_COND
                                                                             (+ (+ COST_BOOLEANOP
                                                                                   (+ (+ COST_CAR
                                                                                         COST_VARREF)
                                                                                      (+ COST_CAR
                                                                                         COST_VARREF)))
                                                                                (let ((x:1
                                                                                  (<_1
                                                                                        (car_1
                                                                                          x)
                                                                                        (car_1
                                                                                          y))))
                                                                                  (if (eq? x:1
                                                                                         'UNKNOWN)
                                                                                      (max
                                                                                        (+ COST_CONS
                                                                                           (+ (+ COST_CAR
                                                                                                 COST_VARREF)
                                                                                              (+ COST_FUNCALL
                                                                                                 (+ (cost_merge
                                                                                                     (cdr x)
                                                                                                     y)
                                                                                                    (+ (+ COST_CDR
                                                                                                          COST_VARREF)
                                                                                                       COST_VARREF)))))
                                                                                        (+ COST_CONS
                                                                                           (+ (+ COST_CAR
                                                                                                 COST_VARREF)
                                                                                              (+ COST_FUNCALL
                                                                                                 (+ (cost_merge
                                                                                                     x
                                                                                                     (cdr y))
                                                                                                    (+ COST_VARREF
                                                                                                       (+ COST_CDR
                                                                                                          COST_VARREF)))))))
                                                                                      (if x:1
                                                                                          (+ COST_CONS
                                                                                             (+ (+ COST_CAR
                                                                                                   COST_VARREF)
                                                                                                (+ COST_FUNCALL
                                                                                                   (+ (cost_merge
                                                                                                       (cdr x)
                                                                                                       y)
                                                                                                      (+ (+ COST_CDR
                                                                                                            COST_VARREF)
                                                                                                         COST_VARREF)))))
                                                                                          (+ COST_CONS
                                                                                             (+ (+ COST_CAR
                                                                                                   COST_VARREF)
                                                                                                (+ COST_FUNCALL
                                                                                                   (+ (cost_merge
                                                                                                       x
                                                                                                       (cdr y))
                                                                                                      (+ COST_VARREF
                                                                                                         (+ COST_CDR
                                                                                                            COST_VARREF)))))))))))))))))
                                                        (if x:1
                                                            COST_VARREF
                                                            (+ COST_COND
                                                               (+ (+ COST_NULL
                                                                     COST_VARREF)
                                                                  (let ((x:1
                                                                    (null?_1
                                                                          y)))
                                                                    (if (eq? x:1
                                                                           'UNKNOWN)
                                                                        (max
                                                                          COST_VARREF
                                                                          (+ COST_COND
                                                                             (+ (+ COST_BOOLEANOP
                                                                                   (+ (+ COST_CAR
                                                                                         COST_VARREF)
                                                                                      (+ COST_CAR
                                                                                         COST_VARREF)))
                                                                                (let ((x:1
                                                                                  (<_1
                                                                                        (car_1
                                                                                          x)
                                                                                        (car_1
                                                                                          y))))
                                                                                  (if (eq? x:1
                                                                                         'UNKNOWN)
                                                                                      (max
                                                                                        (+ COST_CONS
                                                                                           (+ (+ COST_CAR
                                                                                                 COST_VARREF)
                                                                                              (+ COST_FUNCALL
                                                                                                 (+ (cost_merge
                                                                                                     (cdr x)
                                                                                                     y)
                                                                                                    (+ (+ COST_CDR
                                                                                                          COST_VARREF)
                                                                                                       COST_VARREF)))))
                                                                                        (+ COST_CONS
                                                                                           (+ (+ COST_CAR
                                                                                                 COST_VARREF)
                                                                                              (+ COST_FUNCALL
                                                                                                 (+ (cost_merge
                                                                                                     x
                                                                                                     (cdr y))
                                                                                                    (+ COST_VARREF
                                                                                                       (+ COST_CDR
                                                                                                          COST_VARREF)))))))
                                                                                      (if x:1
                                                                                          (+ COST_CONS
                                                                                             (+ (+ COST_CAR
                                                                                                   COST_VARREF)
                                                                                                (+ COST_FUNCALL
                                                                                                   (+ (cost_merge
                                                                                                       (cdr x)
                                                                                                       y)
                                                                                                      (+ (+ COST_CDR
                                                                                                            COST_VARREF)
                                                                                                         COST_VARREF)))))
                                                                                          (+ COST_CONS
                                                                                             (+ (+ COST_CAR
                                                                                                   COST_VARREF)
                                                                                                (+ COST_FUNCALL
                                                                                                   (+ (cost_merge
                                                                                                       x
                                                                                                       (cdr y))
                                                                                                      (+ COST_VARREF
                                                                                                         (+ COST_CDR
                                                                                                            COST_VARREF))))))))))))
                                                                        (if x:1
                                                                            COST_VARREF
                                                                            (+ COST_COND
                                                                               (+ (+ COST_BOOLEANOP
                                                                                     (+ (+ COST_CAR
                                                                                           COST_VARREF)
                                                                                        (+ COST_CAR
                                                                                           COST_VARREF)))
                                                                                  (let ((x:1
                                                                                    (<_1
                                                                                          (car_1
                                                                                            x)
                                                                                          (car_1
                                                                                            y))))
                                                                                    (if (eq? x:1
                                                                                           'UNKNOWN)
                                                                                        (max
                                                                                          (+ COST_CONS
                                                                                             (+ (+ COST_CAR
                                                                                                   COST_VARREF)
                                                                                                (+ COST_FUNCALL
                                                                                                   (+ (cost_merge
                                                                                                       (cdr x)
                                                                                                       y)
                                                                                                      (+ (+ COST_CDR
                                                                                                            COST_VARREF)
                                                                                                         COST_VARREF)))))
                                                                                          (+ COST_CONS
                                                                                             (+ (+ COST_CAR
                                                                                                   COST_VARREF)
                                                                                                (+ COST_FUNCALL
                                                                                                   (+ (cost_merge
                                                                                                       x
                                                                                                       (cdr y))
                                                                                                      (+ COST_VARREF
                                                                                                         (+ COST_CDR
                                                                                                            COST_VARREF)))))))
                                                                                        (if x:1
                                                                                            (+ COST_CONS
                                                                                               (+ (+ COST_CAR
                                                                                                     COST_VARREF)
                                                                                                  (+ COST_FUNCALL
                                                                                                     (+ (cost_merge
                                                                                                         (cdr x)
                                                                                                         y)
                                                                                                        (+ (+ COST_CDR
                                                                                                              COST_VARREF)
                                                                                                           COST_VARREF)))))
                                                                                            (+ COST_CONS
                                                                                               (+ (+ COST_CAR
                                                                                                     COST_VARREF)
                                                                                                  (+ COST_FUNCALL
                                                                                                     (+ (cost_merge
                                                                                                         x
                                                                                                         (cdr y))
                                                                                                        (+ COST_VARREF
                                                                                                           (+ COST_CDR
                                                                                                              COST_VARREF)))))))))))))))))))))))

    (define merge
      (lambda (x y)
        (let ((x:1 (null?_1 x)))
          (if (eq? x:1 'UNKNOWN) (lub_1 y (let ((x:1 (null?_1 y)))
                                            (if (eq? x:1 'UNKNOWN)
                                                (lub_1
                                                  x
                                                  (let ((x:1
                                                    (<_1 (car_1 x) (car_1 y))))
                                                    (if (eq? x:1 'UNKNOWN)
                                                        (lub_1
                                                          (cons (car_1 x)
                                                             (merge
                                                              (cdr_1 x)
                                                              y))
                                                          (cons (car_1 y)
                                                             (merge
                                                              x
                                                              (cdr_1 y))))
                                                        (if x:1
                                                            (cons (car_1 x)
                                                               (merge
                                                                (cdr_1 x)
                                                                y))
                                                            (cons (car_1 y)
                                                               (merge
                                                                x
                                                                (cdr_1
                                                                  y)))))))
                                                (if x:1
                                                    x
                                                    (let ((x:1
                                                      (<_1
                                                            (car_1 x)
                                                            (car_1 y))))
                                                      (if (eq? x:1 'UNKNOWN)
                                                          (lub_1
                                                            (cons (car_1 x)
                                                               (merge
                                                                (cdr_1 x)
                                                                y))
                                                            (cons (car_1 y)
                                                               (merge
                                                                x
                                                                (cdr_1 y))))
                                                          (if x:1
                                                              (cons (car_1 x)
                                                                 (merge
                                                                  (cdr_1 x)
                                                                  y))
                                                              (cons (car_1 y)
                                                                 (merge
                                                                  x
                                                                  (cdr_1
                                                                    y))))))))))
              (if x:1 y (let ((x:1 (null?_1 y)))
                          (if (eq? x:1 'UNKNOWN)
                              (lub_1 x (let ((x:1 (<_1 (car_1 x) (car_1 y))))
                                         (if (eq? x:1 'UNKNOWN)
                                             (lub_1
                                               (cons (car_1 x)
                                                  (merge (cdr_1 x) y))
                                               (cons (car_1 y)
                                                  (merge x (cdr_1 y))))
                                             (if x:1
                                                 (cons (car_1 x)
                                                    (merge (cdr_1 x) y))
                                                 (cons (car_1 y)
                                                    (merge x (cdr_1 y)))))))
                              (if x:1 x (let ((x:1 (<_1 (car_1 x) (car_1 y))))
                                          (if (eq? x:1 'UNKNOWN)
                                              (lub_1
                                                (cons (car_1 x)
                                                   (merge (cdr_1 x) y))
                                                (cons (car_1 y)
                                                   (merge x (cdr_1 y))))
                                              (if x:1
                                                  (cons (car_1 x)
                                                     (merge (cdr_1 x) y))
                                                  (cons (car_1 y)
                                                     (merge
                                                      x
                                                      (cdr_1 y))))))))))))))

    (lambda args
      (cost_mergesort (length_1 (car args))))))
