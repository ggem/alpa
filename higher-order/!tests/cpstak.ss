;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         cpstak.sch
; Description:  continuation-passing version of TAK
; Author:       Will Clinger
; Created:      20-Aug-87
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.
 
(define cpstak
  (lambda (x y z)
    (letrec ([tak (lambda (x y z k)
		    (if (< y x)
		      (tak (- x 1)
			y
			z
			(lambda (v1)
			  (tak (- y 1)
			    z
			    x
			    (lambda (v2)
			      (tak (- z 1)
				x
				y
				(lambda (v3)
				  (tak v1 v2 v3 k)))))))
		      (k z)))])
      (tak x y z (lambda (a) a)))))
 
;;; call: (cpstak 18 12 6)
 
'(cpstak size size size)
