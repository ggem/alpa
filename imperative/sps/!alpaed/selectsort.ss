(define cost-selectsort
  (let ()
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
      (if (eq? x 'unknown)
          'unknown
          (if (eq? y 'unknown)
              $store
              (sa-vector-set! x y z $store))))
    (define (+^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-+ x y $store)))
    (define (-^ x y $store)
      (if (or (eq? y 'unknown) (eq? x 'unknown))
          'unknown
          (sa-- x y $store)))
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
    (define (cost-selectsort array $store)
      (let ([$var:282 (vector-length^ array $store)])
        (c+ '#(0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 5 0 1 1)
          (cost-sort array '0 $var:282))))
    (define (cost-sort array i size)
      (do ([c '#(0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 2 7 1 2 0)
             (c+ c
               '#(0 0 0 0 0 0 0 2 0 2 0 0 2 1 0 0 0 1 0 3 33 1 8 2)
               (cost-min-index (+ i '1) size))]
           [i i (+ i 1)])
          [(= i (- size 1)) c]))
    (define (cost-min-index i size)
      (do ([c '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 6 1 1 0)
             (c+ c '#(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 9 2 1 1)
               '#(0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 2 0 0 12 0 4 0))]
           [i i (+ i 1)])
          [(= i size) c]))
    (lambda (size0)
      (let ([$store (store:new)])
        (let ()
          (let-values ([(vector-size0 $store)
                        (make-vector^ size0 'unknown $store)])
            (cost-vector->exp
              (c+ '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 1)
                (cost-selectsort vector-size0 $store)))))))))
