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


    (define sort
      (lambda (lst)
        (if (null? lst) '() (insert (car lst) (sort (cdr lst))))))

      (define insert
        (lambda (a lst)
          (if (null? lst)
              (cons a '())
              (if (< a (car lst))
                  (cons a lst)
                  (cons (car lst) (insert a (cdr lst)))))))

        (lambda args
          (cost_sort (length_1 (car args))))))
