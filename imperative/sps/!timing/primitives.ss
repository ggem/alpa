#!/bin/tcsh

; exec /usr/local/bin/csi -syntax -script $0 $*

(require-extension posix lolevel)

(define my-cpu-time
  (lambda ()
    (call-with-values cpu-time +)))

(define-syntax while
  (syntax-rules ()
    ((while test body0 body1 ...)
     (let loop ()
       (when test
         body0 body1 ... (loop))))))

;;;
;;;  timing.ss
;;;
;;; procedures to get the time cost of scheme operations
;;;

(define-syntax make-timer
  (syntax-rules ()
    ((timer vars1 vars2)
     (lambda (iterations)
       (let ([start (my-cpu-time)])
         (do ([i 0 (+ i 1)] . vars1)
             [(= i iterations) 'done])
         (let ([middle (my-cpu-time)])
           (do ([i 0 (+ i 1)] . vars2)
               [(= i iterations) 'done])
           (let ([end (my-cpu-time)])
             (let ([time (* (- (+ start end) (+ middle middle))
                           (/ 1000.0 iterations))])
               (printf "~a\n" time)))))))))

(define make-infinite
  (lambda (top)
    (letrec ([make-infinite
               (lambda (lst)
                 (if (null? (cdr lst))
                     (set-cdr! lst top)
                     (make-infinite (cdr lst))))])
      (make-infinite top)
      top)))

(define make-list
  (lambda (size value)
    (letrec ([make-list
               (lambda (size answ)
                 (if (= size 0)
                     answ
                     (make-list (- size 1) (cons value answ))))])
      (make-list size '()))))

(define l0 (make-infinite (list 0)))
(define l1 (make-infinite (make-list 100000 1998)))
(define l2 (make-infinite (list (make-vector 10 3))))
(define l3 (make-infinite
             (do ([i 100000 (- i 1)]
                  [answ '() (cons (make-vector 3 2) answ)])
                 [(= i 0) answ])))
(define f0 (lambda (x y) (eq? x y)))

(define cost_cons
  (make-timer
    ([v l0 (cdr v)] [f #f f]) ([v l1 (cdr v)] [f #f (begin (cons f v) f)])))

(define cost_car
  (make-timer
    ([v l0 (cdr v)] [f #t f]) ([v l1 (cdr v)] [f #t (car v)])))

(define cost_cdr
  (make-timer
    ([v l0 v]) ([v l1 (cdr v)])))

(define cost_set-car!
  (make-timer
    ([v l0 (cdr v)] [f #f f])
    ([v l1 (cdr v)] [f #f (begin (set-car! v 1998) f)])))

(define cost_set-cdr!
  (make-timer
    ([v l0 (cdr v)] [f #f (let ([cv (cdr v)]) cv)])
    ([v l1 (cdr v)] [f #f (let ([cv (cdr v)]) (set-cdr! v cv) cv)])))

(define cost_null?
  (make-timer
    ([v l0 (cdr v)] [f 0 (car v)])
    ([v l1 (cdr v)] [f 0 (null? (car v))])))

(define cost_make-vector
  (make-timer
    ([v l0 (cdr v)] [f 20 20])
    ([v l1 (cdr v)] [f 20 (begin (make-vector 5) 20)])))

(define cost_vector
  (make-timer
    ([v l0 (cdr v)] [f 20 20])
    ([v l1 (cdr v)] [f 20 (begin (vector 5 6) 20)])))

(define cost_vector-ref
  (make-timer
    ([v l2 (cdr v)] [f 2 (begin (car v) f)])
    ([v l3 (cdr v)] [f 2 (vector-ref (car v) f)])))

(define cost_vector-length
  (make-timer
    ([v l2 (cdr v)] [f 2 (begin (car v) f)])
    ([v l3 (cdr v)] [f 2 (begin (vector-length (car v)) f)])))

(define cost_vector-set!
  (make-timer
    ([v l2 (cdr v)] [f 2 (begin (car v) f)])
    ([v l3 (cdr v)] [f 2 (begin (vector-set! (car v) 0 f) f)])))

(define cost_eq?
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin  (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (eq? (car v) (cadr v))])))

(define cost_+
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin  (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (+ (car v) (cadr v))])))

(define cost_-
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin  (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (- (car v) (cadr v))])))

(define cost_*
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin  (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (* (car v) (cadr v))])))

(define cost_quotient
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin  (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (quotient (car v) (cadr v))])))

(define cost_>
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin  (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (> (car v) (cadr v))])))

(define cost_<
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin  (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (< (car v) (cadr v))])))

(define cost_=
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin  (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (= (car v) (cadr v))])))

(define cost_const
  (make-timer
    ([v l0 (cdr v)] [f l0 (cdr v)])
    ([v l1 (cdr v)] [f l0 (begin (cdr v) '())])))

(define cost_varref
  (make-timer
    ([v l0 (cdr v)])
    ([v l1 (begin (cdr v) v)])))

(define cost_if
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin (number? f) (car v))])
    ([v l1 (cdr v)] [f 0 (if (number? f) (car v) (cadr v))])))

(define cost_let
  (make-timer
    ([v l0 (cdr v)] [f 0 (begin (car v) (boolean? f))])
    ([v l1 (cdr v)] [f 0 (let ([x (car v)]) (boolean? x))])))

(define cost_funcall
  (make-timer
    ([v l0 (cdr v)] [f 0 (eq? (car v) (cadr v))])
    ([v l1 (cdr v)] [f 0 (f0 (car v) (cadr v))])))

(define cost_set!
  (make-timer
    ([v l0 (cdr v)] [f 0 f])
    ([v l1 (cdr v)] [f 0 (begin (set! f 0) f)])))

(define cost_while
  (make-timer
    ([v l0 (cdr v)] [f #f f])
    ([v l1 (cdr v)] [f #f (begin (while f f) f)])))

(define primitives
  (list->vector
    (map (lambda (name)
           (let ([cost_name (string->symbol (sprintf "cost_~a" name))])
             `(,name ,cost_name . ,(global-ref cost_name))))
      '(cons car cdr set-car! set-cdr! null?
         make-vector vector vector-ref vector-length vector-set!
         eq?  + - * quotient > < =
         const varref if let funcall set! while))))

(define (main args)
  (let ([loops 10]
        [iterations 100000])
    (case (length args)
      [(1)
       (let ([index (string->number (car args))])
         (if (and (number? index) (< -1 index (vector-length primitives)))
             (let ([entry (vector-ref primitives index)])
               (let ([timer (cddr entry)])
                 (timer iterations)))))]
      [(0)
       (let ([vec (make-vector (vector-length primitives) '())])
         (do ([i loops (- i 1)])
             [(zero? i)]
           (do ([index (- (vector-length primitives) 1) (- index 1)])
               [(< index 0) 'done]
             (let ([name (car (vector-ref primitives index))])
               (printf "; ~a	->	" name)
               (flush-output)
               (let ([time (with-input-from-pipe
                             (sprintf "./primitives ~a" index)
                             read)])
                 (printf "~a\n" time)
                 (vector-set! vec index (cons time (vector-ref vec index)))))))
         (do ([index 0 (+ index 1)])
             [(= index (vector-length primitives)) 'done]
           (let ([cost-name (cadr (vector-ref primitives index))]
                 [times (vector-ref vec index)])
             (printf "(define ~a ~a)	; ~a\n" cost-name
               (/ (apply + times) loops)
               times))))])))


(main (command-line-arguments))

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "csi -syntax"
;; End:

