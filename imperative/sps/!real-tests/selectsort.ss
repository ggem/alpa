;;


(define (selectsort array)
  (sort array 0 (vector-length array))
  array)

(define (sort array i size)
  (if (< i (- size 1))
      (let ([min-i (min-index array (+ i 1) i size)])
        (let ([tmp (vector-ref array min-i)])
          (vector-set! array min-i (vector-ref array i))
          (vector-set! array i tmp)
          (sort array (+ i 1) size)))))

(define (min-index array i min-i size)
  (if (< i size)
      (if (< (vector-ref array i) (vector-ref array min-i))
          (min-index array (+ i 1) i size)
          (min-index array (+ i 1) min-i size))
      min-i))

;

'(selectsort (make-vector-unknowns 'iota))
