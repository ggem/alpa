#!/bin/tcsh

; exec /usr/local/bin/csi -syntax -script $0 $*

(require-extension posix lolevel)

(define my-cpu-time
  (lambda ()
    (call-with-values cpu-time +)))

(define box list)
(define unbox car)
(define set-box! set-car!)
(define format sprintf)
(define top-level-value global-ref)
(define (vector-copy v)
  (let ([v2 (make-vector (vector-length v))])
    (vector-copy! v v2)
    v2))
(define make-list
  (lambda (size value)
    (letrec ([make-list
               (lambda (size answ)
                 (if (= size 0)
                     answ
                     (make-list (- size 1) (cons value answ))))])
      (make-list size '()))))
(define-syntax record-case
  (syntax-rules ()
    [(_ var ...) (syntax 0)]))

(include "../sps.ss")
(include "../alpa.ss")
(include "../!alpaed/insertsort.ss")
(include "../!alpaed/merge.ss")
(include "../!alpaed/mergesort.ss")
(include "../!alpaed/reverse.ss")
(include "../!alpaed/selectsort.ss")
(include "../!alpaed/vector-sum.ss")



;;
;; main
;;
(define (main args)
  (let ([procedures `(;(insertsort 	. ,cost-insertsort)
                      (list-mergesort	. ,cost-list-mergesort)
                      (mergesort	. ,cost-mergesort)
                      ;(reverse!		. ,cost-reverse!)
                      ;(selectsort	. ,cost-selectsort)
                      ;(vector-sum	. ,cost-vector-sum)
		     )]
        [sizes '(10 20 50 100 200 300 500 1000 2000)]
        [min-time 10000])
    (case (length args)
      [(3)
       (let ([proc (string->symbol (car args))]
             [size (string->number (cadr args))]
             [num-loops (string->number (caddr args))])
         (let ([name+proc (assq proc procedures)])
           (if (and name+proc size num-loops)
               (let ([start-time (my-cpu-time)])
                 (let ([answer ((cdr name+proc) size)])
                   (do ([i (- num-loops 1) (- i 1)])
                       [(= i 0) 'done]
                     ((cdr name+proc) size))
                   (let ([end-time (my-cpu-time)])
                     (pp `(,(/ (- end-time start-time) (+ num-loops 0.0)) . ,answer))
                     (exit)))))))]
      [(0)
       (do ([sizes sizes (cdr sizes)]
            [i 0 (+ i 1)])
           [(null? sizes) 'done]
         (do ([procs (map car procedures) (cdr procs)]
              [j 0 (+ j 1)])
             [(null? procs) 'done]
           (do ([loops 1 (* loops 2)]
                [time+answ '(0 . (+))
                  (with-input-from-pipe
                    (sprintf "./alpaed ~a ~a ~a"
                      (car procs) (car sizes) loops)
                    read)])
               [(>= (* (car time+answ) loops) min-time)
                (pp `((,(car procs) ,(car sizes)) ===> ,(cdr time+answ)))
                (printf "; time ~a	~a:	~a ms\n\n" (car procs) (car sizes)(car time+answ))
                (flush-output)])))
       (exit)])
    (fprintf (current-error-port)
      "prog [<procedure> <size> <loops> | run-procedures]\n")
    (exit 1)))

(main (command-line-arguments))

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "csi -syntax"
;; End:
