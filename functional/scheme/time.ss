;;;
;;;  time.ss
;;;
;;; procedures to handle runtime and garbage collection time
;;;

(define time->runtime car)
(define time->runtime+gc cdr)
(define make-time cons)

(define run-time		;;; returns a pair (program . program+gc)
  (lambda ()
    (let ([stt (statistics)])
      (let ([cpu-time (vector-ref stt 1)]
	    [gc-time (vector-ref stt 5)])
	(make-time (- cpu-time gc-time) cpu-time)))))

(define time-add
  (lambda (t1 t2)
    (make-time
      (+ (time->runtime t1)    (time->runtime t2))
      (+ (time->runtime+gc t1) (time->runtime+gc t2)))))

(define time-minus
  (lambda (t1 t2)
    (make-time
      (- (time->runtime t1)    (time->runtime t2))
      (- (time->runtime+gc t1) (time->runtime+gc t2)))))

(define time-divide
  (lambda (t n)
    (make-time (/ (time->runtime t) n) (/ (time->runtime+gc t) n))))

(define time-multiply
  (lambda (t n)
    (make-time (* (time->runtime t) n) (* (time->runtime+gc t) n))))

