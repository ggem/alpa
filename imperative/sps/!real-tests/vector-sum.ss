;

(define (vector-sum v)
  (let ([sum 0])
    (let ([i 0])
      (begin
        (while (< i (vector-length v))
          (begin
            (set! sum (+ sum (vector-ref v i)))
            (set! i (+ i 1))))
        sum))))

'(vector-sum (make-vector-unknowns '23))
