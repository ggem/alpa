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
              (ms a b start half1))
          (if (> half2 1)
              (ms a b end1 half2))
          (copy a b start start size)
          (merge a b start half1 half2))))))

(define (merge a b start size1 size2)
  (let ([index start])
    (let ([i1 start])
      (let ([end1 (+ i1 size1)])
        (let ([i2 end1])
          (let ([end2 (+ start (+ size1 size2))])
            (while (if (< i1 end1) (< i2 end2) #f)
              (if (< (vector-ref b i1) (vector-ref b i2))
                  (begin
                    (vector-set! a index (vector-ref b i1))
                    (set! i1 (+ i1 1)))
                  (begin
                    (vector-set! a index (vector-ref b i2))
                    (set! i2 (+ i2 1))))
              (set! index (+ index 1)))
            (copy b a i1 index (- end1 i1))
            (copy b a i2 index (- end2 i2))))))))

(define (copy src dst start-src start-dst size)
  (if (> size 0)
      (begin
        (vector-set! dst start-dst (vector-ref src start-src))
        (copy src dst (+ start-src 1) (+ start-dst 1) (- size 1)))))

'(mergesort (make-vector-unknowns 'mergesort))
