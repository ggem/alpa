;;

(define (insertsort array)
  (isort array 1 (vector-length array))
  array)

(define (isort array i size)
  (if (< i size)
      (begin
        (insert array i (vector-ref array i))
        (isort array (+ i 1) size)))
  array)

(define (insert array i item)
  (if (> i 0)
      (let ([tmp (vector-ref array (- i 1))])
        (if (< item tmp)
            (begin
              (vector-set! array i tmp)
              (insert array (- i 1) item))
            (begin
              (vector-set! array i item)
              array)))
      (begin
        (vector-set! array i item)
        array)))

'(insertsort (make-vector-unknowns 'reverse-iota))
