(define cost-vector-sum
  (let ()
    (define cons^ sa-cons)
    (define (car^ x $store)
      (if (or (eq? x 'unknown)) 'unknown (sa-car x $store)))
    (define (cdr^ x $store)
      (if (or (eq? x 'unknown)) 'unknown (sa-cdr x $store)))
    (define (set-car!^ x y $store)
      (if (or (eq? x 'unknown))
          'unknown
          (sa-set-car! x y $store)))
    (define (set-cdr!^ x y $store)
      (if (or (eq? x 'unknown))
          'unknown
          (sa-set-cdr! x y $store)))
    (define (make-vector^ x y $store)
      (if (or (eq? x 'unknown))
          'unknown
          (sa-make-vector x y $store)))
    (define vector^ sa-vector)
    (define (vector-ref^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-vector-ref x y $store)))
    (define (vector-length^ x $store)
      (if (or (eq? x 'unknown))
          'unknown
          (sa-vector-length x $store)))
    (define (vector-set!^ x y z $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-vector-set! x y z $store)))
    (define (null?^ x $store)
      (if (or (eq? x 'unknown)) 'unknown (sa-null? x $store)))
    (define (eq?^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-eq? x y $store)))
    (define (+^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-+ x y $store)))
    (define (-^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-- x y $store)))
    (define (*^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-* x y $store)))
    (define (quotient^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-quotient x y $store)))
    (define (>^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-> x y $store)))
    (define (<^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-< x y $store)))
    (define (=^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-= x y $store)))
    (define (cost-vector-sum v $store)
      (let ([ii (vector-length^ v $store)])
        (do ([i ii (- i 1)]
             [c '#(0 0 0 0 0 0 2 2 1 0 0 0 0 0 0 0 0 1 0 5 18 1 3 1)
               (c+ c '#(0 0 0 0 0 0 0 5 1 2 0 0 2 0 0 0 0 1 0 7 32 1 11 1))])
            [(= i 0) c])))
    (lambda (size0)
      (let ([$store (store:new)])
        (let ()
          (let-values ([(vector-size0 $store)
                        (make-vector^ size0 'unknown $store)])
            (cost-vector->exp
              (c+ '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1)
                (cost-vector-sum vector-size0 $store)))))))))
