;;
;;

(define (l)
  (parameterize ([current-directory "/Users/ggem/Documents/ggem/alpa/imperative/sps"])
    (load "sps.ss")
    (load "alpa.ss")))

(l)

(define (trace-alpa)
  (trace alpa alpa-body normalize t))

(define (untrace-alpa)
  (untrace alpa alpa-body normalize t))

(define (trace-costs)
  (trace c+ cmax c* cost-vector->exp))

(define (untrace-costs)
  (untrace c+ cmax c* cost-vector->exp))

(define (trace-sa)
  (trace sa-cons sa-car sa-cdr sa-set-car!  sa-set-cdr!  sa-make-vector
   sa-vector sa-vector-ref sa-vector-length sa-vector-set! sa-null? sa-eq?
   sa-+ sa-- sa-* sa-quotient sa-> sa-< sa-=))

(define (untrace-sa)
  (untrace sa-cons sa-car sa-cdr sa-set-car!  sa-set-cdr!  sa-make-vector
   sa-vector sa-vector-ref sa-vector-length sa-vector-set! sa-null? sa-eq?
   sa-+ sa-- sa-* sa-quotient sa-> sa-< sa-=))

(define (trace-simp)
  (trace simplify-pairs simplify-add simplify))

(define (untrace-simp)
  (untrace simplify-pairs simplify-add simplify))

(define (trace-store)
  (trace store:new store:heap-pointer store:update store:allocate store:lookup))

(define (untrace-store)
  (untrace store:new store:heap-pointer store:update store:allocate store:lookup))

(define (trace-all)
  (append
    (trace-alpa)
    (trace-costs)
    (trace-sa)
    (trace-simp)
    (trace-store)))

(define (untrace-all)
  (append
    (untrace-alpa)
    (untrace-costs)
    (untrace-sa)
    (untrace-simp)
    (untrace-store)))
