;;

(define (mergesort array)
  (let ([size (vector-length array)])
    (let ([scratch (make-vector size 0)])
      (ms array scratch 0 size)
      array)))

(define (ms a b start size)
  (let ([half1 (quotient size 2)])
    (let ([half2 (- size half1)])
      (let ([end1 (+ start half1)])
        (let ([end2 (+ start size)])
          (if (> half1 1)
              (begin
                (ms a b start half1)
                'void))
          (if (> half2 1)
              (begin
                (ms a b end1 half2)
                'void))
          (copy a b start start size)
          (merge a b start half1 half2)
          a)))))

(define (merge a b start size1 size2)
  (let ([index (vector start)]
        [end1 (+ start size1)]
        [end2 (+ start (+ size1 size2))]
        [i1 (vector start)])
    (let ([i2 (vector end1)])
      (merge2lists a b index i1 i2 end1 end2)
      (let ([i1v (vector-ref i1 0)]
            [i2v (vector-ref i2 0)]
            [indexv (vector-ref index 0)])
        (copy b a i1v indexv (- end1 i1v))
        (copy b a i2v indexv (- end2 i2v))))))

(define (merge2lists a b index i1 i2 end1 end2)
  (let ([i1v (vector-ref i1 0)]
        [i2v (vector-ref i2 0)]
        [indexv (vector-ref index 0)])
    (if (< i1v end1)
        (if (< i2v end2)
            (begin
              (if (< (vector-ref b i1v) (vector-ref b i2v))
                  (begin
                    (vector-set! a indexv (vector-ref b i1v))
                    (vector-set! i1 0 (+ i1v 1))
                    a)
                  (begin
                    (vector-set! a indexv (vector-ref b i2v))
                    (vector-set! i2 0 (+ i2v 1))
                    a))
              (vector-set! index 0 (+ indexv 1))
              (merge2lists a b index i1 i2 end1 end2)
              a)))))

(define (copy src dst start-src start-dst size)
  (if (> size 0)
      (begin
        (vector-set! dst start-dst (vector-ref src start-src))
        (copy src dst (+ start-src 1) (+ start-dst 1) (- size 1))
        src)))

'(mergesort (make-vector-unknowns 'mergesort))