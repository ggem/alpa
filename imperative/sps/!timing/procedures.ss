#!/bin/tcsh

; exec /usr/local/bin/csi -syntax -script $0 $*

(require-extension posix)

(define my-cpu-time
  (lambda ()
    (call-with-values cpu-time +)))

(define-syntax while
  (syntax-rules ()
    ((while test body0 body1 ...)
     (let loop ()
       (when test
         body0 body1 ... (loop))))))

(define make-worstcase
  (letrec ([mergesort-worstcase
             (lambda (size)
               (let ([v (make-vector size #f)])
                 (vector-set! v (- size 1) (- size 1))
                 (let loop ([start 0] [size size] [n (- size 2)])
                   (if (> size 1)
                       (let ([mid (quotient size 2)])
                         (vector-set! v (+ start mid -1) n)
                         (loop start mid (- n 1))
                         (loop (+ start mid) (- size mid) (- n mid)))))
                 v))]
           [iota
             (lambda (n)
               (iota-hlp (- n 1) '()))]
           [iota-hlp
             (lambda (n acc)
               (if (< n 0)
                   acc
                   (iota-hlp (- n 1) (cons n acc))))]
           [reverse-iota
             (lambda (n)
               (if (= n 0)
                   '()
                   (cons n (reverse-iota (- n 1)))))]
           [compose
             (lambda (f g)
               (lambda (x) (f (g x))))])
    (lambda (kind)
      (case kind
        [(vector-mergesort) mergesort-worstcase]
        [(list-mergesort) (compose vector->list mergesort-worstcase)]
        [(reverse!) iota]
        [(insertsort selectsort vector-sum)
         (compose list->vector reverse-iota)]
        [else (error 'make-worstcase "Unknown procedure: " kind)]))))

(define copy
  (letrec ([copy-vector
             (lambda (v0)
               (let ([size (vector-length v0)])
                 (let ([v1 (make-vector size)])
                   (do ([i (- size 1) (- i 1)])
                       [(< i 0) v1]
                     (vector-set! v1 i (vector-ref v0 i))))))]
           [copy-list
             (lambda (ls)
               (if (null? ls)
                   '()
                   (cons (car ls) (copy-list (cdr ls)))))])
    (lambda (obj)
      (if (vector? obj)
          (copy-vector obj)
          (copy-list obj)))))


;; insertsort
;;
(define vector-insertsort
  (let ()
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
    insertsort))

;; list-mergesort
;;
(define list-mergesort
  (let ()
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
    mergesort))

;; vector-mergesort
;;
(define vector-mergesort
  (let ()
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
    mergesort))

;; reverse!
;;
(define list-reverse!
  (let ()
    (define (reverse! ls)
      (if (null? ls)
          ls
          (reverse!-loop '() ls)))
    (define (reverse!-loop prev curr)
      (let ([next (cdr curr)])
        (set-cdr! curr prev)
        (if (null? next)
            curr
            (reverse!-loop curr next))))
    reverse!))

;; selectsort
;;
(define vector-selectsort
  (let ()
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
    selectsort))

;; vector-sum
;; 
(define (vector-sum v)
  (let ([sum 0])
    (let ([i 0])
      (begin
        (while (< i (vector-length v))
          (begin
            (set! sum (+ sum (vector-ref v i)))
            (set! i (+ i 1))))
        sum))))

;;
;; main
;;
(define (main args)
  (let ([procedures `((insertsort 	. ,vector-insertsort)
                      (list-mergesort	. ,list-mergesort)
                      (vector-mergesort . ,vector-mergesort)
                      (reverse!		. ,list-reverse!)
                      (selectsort	. ,vector-selectsort)
                      (vector-sum	. ,vector-sum))]
        [sizes '(10 20 50 100 200 300 500 1000 2000)]
        [min-time 10000]
        [num-big-loops 10])
    (case (length args)
      [(3)
       (let ([proc (string->symbol (car args))]
             [size (string->number (cadr args))]
             [num-loops (string->number (caddr args))])
         (let ([name+proc (assq proc procedures)])
           (if (and name+proc size num-loops)
               (let ([original ((make-worstcase proc) size)])
                 (let ([start-time (my-cpu-time)])
                   (do ([i num-loops (- i 1)])
                       [(= i 0) 'done]
                     ((cdr name+proc) (copy original)))
                   (let ([end-time (my-cpu-time)])
                     (do ([i num-loops (- i 1)])
                         [(= i 0) 'done]
                       (copy original))
                     (let ([extra-time (my-cpu-time)])
                       (printf "~a\n"
                         (/ (- (+ end-time end-time)
                              (+ start-time extra-time))
                           (+ num-loops 0.0)))
                       (exit))))))))]
      [(1)
       (if (string=? (car args) "run-procedures")
           (let ([times (make-vector (length sizes))])
             (do ([i (- (length sizes) 1) (- i 1)])
                 [(< i 0) 'done]
               (vector-set! times i (make-vector (length procedures) 0)))
             (do ([n num-big-loops (- n 1)])
                 [(= n 0) 'done]
               (do ([sizes sizes (cdr sizes)]
                    [i 0 (+ i 1)])
                   [(null? sizes) 'done]
                 (do ([procs (map car procedures) (cdr procs)]
                      [j 0 (+ j 1)])
                     [(null? procs) 'done]
                   (do ([loops 1 (* loops 2)]
                        [time 0
                          (with-input-from-pipe
                            (sprintf "./procedures ~a ~a ~a"
                              (car procs) (car sizes) loops)
                            read)])
                       [(>= (* time loops) min-time)
                        (vector-set! (vector-ref times i) j
                          (+ (vector-ref (vector-ref times i) j)
                            time))
                        (printf "; ~a	~a	-> ~a ms\n"
                          (car procs) (car sizes) time)]))))
             (printf ";\n;\n;\n")
             (do ([sizes sizes (cdr sizes)]
                  [i 0 (+ i 1)])
                 [(null? sizes) 'done]
               (do ([procs (map car procedures) (cdr procs)]
                    [j 0 (+ j 1)])
                   [(null? procs) 'done]
                 (printf "; ~a	~a	-> ~a ms\n"
                   (car procs)
                   (car sizes)
                   (/ (vector-ref (vector-ref times i) j) num-big-loops))))
             (exit)))])
    (fprintf (current-error-port)
      "prog [<procedure> <size> <loops> | run-procedures]\n")
    (exit 1)))

(main (command-line-arguments))

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "csi -syntax"
;; End:
