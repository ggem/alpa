;

(define (mergesort ls)
  (if (null? ls)
    '()
    (dosort ls (length ls))))

(define (dosort ls n)
  (if (= n 1)
      (cons (car ls) '())
      (if (= n 2)
          (let ([x (car ls)])
            (let ([y (car (cdr ls))])
              (if (< x y)
                  (cons x (cons y '()))
                  (cons y (cons x '())))))
          (let ([i (quotient n 2)])
            (domerge
              (dosort ls i)
              (dosort (list-tail ls i) (- n i)))))))

(define (domerge ls1 ls2)
  (if (null? ls1)
      ls2
      (if (null? ls2)
          ls1
          (if (< (car ls1) (car ls2))
              (begin
                (set-cdr! ls1 (domerge (cdr ls1) ls2))
                ls1)
              (begin
                (set-cdr! ls2 (domerge ls1 (cdr ls2)))
                ls2)))))

(define (list-tail ls i)
  (if (= i 0)
      ls
      (list-tail (cdr ls) (- i 1))))

(define (length ls)
  (if (null? ls)
      0
      (+ 1 (length (cdr ls)))))

'(mergesort (make-list-unknowns 'mergesort))
