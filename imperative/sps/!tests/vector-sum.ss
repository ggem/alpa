;

(define (vector-sum v)
  (let ([sum (vector 0)])
    (let ([i (vector 0)])
      (begin
        (vector-sum-loop sum i v)
        (vector-ref sum 0)))))

(define (vector-sum-loop sum i v)
  (if (< (vector-ref i 0) (vector-length v))
      (begin
        (vector-set! sum 0
          (+ (vector-ref sum 0) (vector-ref v (vector-ref i 0))))
        (vector-set! i  0 (+ (vector-ref i 0) 1))
        (vector-sum-loop sum i v))
      'void))

'(vector-sum (make-vector-unknowns '23))
