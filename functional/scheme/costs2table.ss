;;;
;;;
;;;

(let ([titles '(const varref car cdr cons if null? equal? let funcall)])
  (display "program		size")
  (for-each (lambda (x) (display #\tab) (display x)) titles)
  (newline))

(define-syntax print
  (let ([names '((cost_constant		0 . constant)
		 (cost_varref		1 . varref)
		 (cost_car		2 . car)
		 (cost_cdr		3 . cdr)
		 (cost_cons		4 . cons)
		 (cost_cond		5 . if)
		 (cost_null		6 . null?)
		 (cost_booleanop	7 . equality)
		 (cost_binding		8 . let)
		 (cost_funcall		9 . funcall))])
    (let ([update-cost
	    (lambda (cost vec)
	      (if (symbol? cost)
		(vector-set! vec (cadr (assq cost names)) 1)
		(vector-set! vec (cadr (assq (caddr cost) names))
		  (cadr cost))))])
      (letrec ([costs->table-row
		 (lambda (costs)
		   (let ([vec (make-vector (length names))])
		     (for-each (lambda (cost) (update-cost cost vec)) costs)
		     (do ([i 0 (+ i 1)]
			  [str "" (format "~a	~a" str (vector-ref vec i))])
		       ((= i 10) str))))])
	(lambda (x)
	  (syntax-case x (quote +)
	    [(_ (quote name) size (+ cost0 ...))
	     (let* ([costs (syntax-object->datum (syntax (cost0 ...)))]
		    [row (costs->table-row costs)])
	       (with-syntax ([r (format "~a    	~a~a~%"
				  (syntax-object->datum (syntax name))
				  (syntax-object->datum (syntax size))
				  row)])
		 (syntax (display r))))]))))))

(print
  'insertsort
  10
  (+ (* 321 cost_varref)
     (* 66 cost_null)
     (* 65 cost_funcall)
     (* 11 cost_constant)
     (* 55 cost_cons)
     (* 111 cost_cond)
     (* 55 cost_cdr)
     (* 100 cost_car)
     (* 45 cost_booleanop)))

(print
  'insertsort
  20
  (+ (* 1241 cost_varref)
     (* 231 cost_null)
     (* 230 cost_funcall)
     (* 21 cost_constant)
     (* 210 cost_cons)
     (* 421 cost_cond)
     (* 210 cost_cdr)
     (* 400 cost_car)
     (* 190 cost_booleanop)))

(print
  'insertsort
  50
  (+ (* 7601 cost_varref)
     (* 1326 cost_null)
     (* 1325 cost_funcall)
     (* 51 cost_constant)
     (* 1275 cost_cons)
     (* 2551 cost_cond)
     (* 1275 cost_cdr)
     (* 2500 cost_car)
     (* 1225 cost_booleanop)))

(print
  'insertsort
  100
  (+ (* 30201 cost_varref)
     (* 5151 cost_null)
     (* 5150 cost_funcall)
     (* 101 cost_constant)
     (* 5050 cost_cons)
     (* 10101 cost_cond)
     (* 5050 cost_cdr)
     (* 10000 cost_car)
     (* 4950 cost_booleanop)))

(print
  'insertsort
  200
  (+ (* 120401 cost_varref)
     (* 20301 cost_null)
     (* 20300 cost_funcall)
     (* 201 cost_constant)
     (* 20100 cost_cons)
     (* 40201 cost_cond)
     (* 20100 cost_cdr)
     (* 40000 cost_car)
     (* 19900 cost_booleanop)))

(print
  'insertsort
  300
  (+ (* 270601 cost_varref)
     (* 45451 cost_null)
     (* 45450 cost_funcall)
     (* 301 cost_constant)
     (* 45150 cost_cons)
     (* 90301 cost_cond)
     (* 45150 cost_cdr)
     (* 90000 cost_car)
     (* 44850 cost_booleanop)))

(print
  'insertsort
  500
  (+ (* 751001 cost_varref)
     (* 125751 cost_null)
     (* 125750 cost_funcall)
     (* 501 cost_constant)
     (* 125250 cost_cons)
     (* 250501 cost_cond)
     (* 125250 cost_cdr)
     (* 250000 cost_car)
     (* 124750 cost_booleanop)))

(print
  'insertsort
  1000
  (+ (* 3002001 cost_varref)
     (* 501501 cost_null)
     (* 501500 cost_funcall)
     (* 1001 cost_constant)
     (* 500500 cost_cons)
     (* 1001001 cost_cond)
     (* 500500 cost_cdr)
     (* 1000000 cost_car)
     (* 499500 cost_booleanop)))

(print
  'insertsort
  2000
  (+ (* 12004001 cost_varref)
     (* 2003001 cost_null)
     (* 2003000 cost_funcall)
     (* 2001 cost_constant)
     (* 2001000 cost_cons)
     (* 4002001 cost_cond)
     (* 2001000 cost_cdr)
     (* 4000000 cost_car)
     (* 1999000 cost_booleanop)))

(print
  'selectsort
  10
  (+ (* 576 cost_varref)
     (* 121 cost_null)
     (* 120 cost_funcall)
     (* 11 cost_constant)
     (* 55 cost_cons)
     (* 211 cost_cond)
     (* 200 cost_cdr)
     (* 190 cost_car)
     (* 90 cost_booleanop)
     (* 55 cost_binding)))

(print
  'selectsort
  20
  (+ (* 2251 cost_varref)
     (* 441 cost_null)
     (* 440 cost_funcall)
     (* 21 cost_constant)
     (* 210 cost_cons)
     (* 821 cost_cond)
     (* 800 cost_cdr)
     (* 780 cost_car)
     (* 380 cost_booleanop)
     (* 210 cost_binding)))

(print
  'selectsort
  50
  (+ (* 13876 cost_varref)
     (* 2601 cost_null)
     (* 2600 cost_funcall)
     (* 51 cost_constant)
     (* 1275 cost_cons)
     (* 5051 cost_cond)
     (* 5000 cost_cdr)
     (* 4950 cost_car)
     (* 2450 cost_booleanop)
     (* 1275 cost_binding)))

(print
  'selectsort
  100
  (+ (* 55251 cost_varref)
     (* 10201 cost_null)
     (* 10200 cost_funcall)
     (* 101 cost_constant)
     (* 5050 cost_cons)
     (* 20101 cost_cond)
     (* 20000 cost_cdr)
     (* 19900 cost_car)
     (* 9900 cost_booleanop)
     (* 5050 cost_binding)))

(print
  'selectsort
  200
  (+ (* 220501 cost_varref)
     (* 40401 cost_null)
     (* 40400 cost_funcall)
     (* 201 cost_constant)
     (* 20100 cost_cons)
     (* 80201 cost_cond)
     (* 80000 cost_cdr)
     (* 79800 cost_car)
     (* 39800 cost_booleanop)
     (* 20100 cost_binding)))

(print
  'selectsort
  300
  (+ (* 495751 cost_varref)
     (* 90601 cost_null)
     (* 90600 cost_funcall)
     (* 301 cost_constant)
     (* 45150 cost_cons)
     (* 180301 cost_cond)
     (* 180000 cost_cdr)
     (* 179700 cost_car)
     (* 89700 cost_booleanop)
     (* 45150 cost_binding)))

(print
  'selectsort
  500
  (+ (* 1376251 cost_varref)
     (* 251001 cost_null)
     (* 251000 cost_funcall)
     (* 501 cost_constant)
     (* 125250 cost_cons)
     (* 500501 cost_cond)
     (* 500000 cost_cdr)
     (* 499500 cost_car)
     (* 249500 cost_booleanop)
     (* 125250 cost_binding)))

(print
  'selectsort
  1000
  (+ (* 5502501 cost_varref)
     (* 1002001 cost_null)
     (* 1002000 cost_funcall)
     (* 1001 cost_constant)
     (* 500500 cost_cons)
     (* 2001001 cost_cond)
     (* 2000000 cost_cdr)
     (* 1999000 cost_car)
     (* 999000 cost_booleanop)
     (* 500500 cost_binding)))

(print
  'selectsort
  2000
  (+ (* 22005001 cost_varref)
     (* 4004001 cost_null)
     (* 4004000 cost_funcall)
     (* 2001 cost_constant)
     (* 2001000 cost_cons)
     (* 8002001 cost_cond)
     (* 8000000 cost_cdr)
     (* 7998000 cost_car)
     (* 3998000 cost_booleanop)
     (* 2001000 cost_binding)))

(print
  'mergesort
  10
  (+ (* 456 cost_varref)
     (* 192 cost_null)
     (* 138 cost_funcall)
     (* 28 cost_constant)
     (* 69 cost_cons)
     (* 217 cost_cond)
     (* 112 cost_cdr)
     (* 119 cost_car)
     (* 25 cost_booleanop)))

(print
  'mergesort
  20
  (+ (* 1154 cost_varref)
     (* 468 cost_null)
     (* 340 cost_funcall)
     (* 58 cost_constant)
     (* 177 cost_cons)
     (* 537 cost_cond)
     (* 284 cost_cdr)
     (* 315 cost_car)
     (* 69 cost_booleanop)))

(print
  'mergesort
  50
  (+ (* 3680 cost_varref)
     (* 1440 cost_null)
     (* 1054 cost_funcall)
     (* 148 cost_constant)
     (* 573 cost_cons)
     (* 1677 cost_cond)
     (* 908 cost_cdr)
     (* 1047 cost_car)
     (* 237 cost_booleanop)))

(print
  'mergesort
  100
  (+ (* 8562 cost_varref)
     (* 3284 cost_null)
     (* 2412 cost_funcall)
     (* 298 cost_constant)
     (* 1345 cost_cons)
     (* 3857 cost_cond)
     (* 2116 cost_cdr)
     (* 2491 cost_car)
     (* 573 cost_booleanop)))

(print
  'mergesort
  200
  (+ (* 19526 cost_varref)
     (* 7372 cost_null)
     (* 5428 cost_funcall)
     (* 598 cost_constant)
     (* 3089 cost_cons)
     (* 8717 cost_cond)
     (* 4832 cost_cdr)
     (* 5779 cost_car)
     (* 1345 cost_booleanop)))

(print
  'mergesort
  300
  (+ (* 31354 cost_varref)
     (* 11748 cost_null)
     (* 8660 cost_funcall)
     (* 898 cost_constant)
     (* 4977 cost_cons)
     (* 13937 cost_cond)
     (* 7764 cost_cdr)
     (* 9355 cost_car)
     (* 2189 cost_booleanop)))

(print
  'mergesort
  500
  (+ (* 56354 cost_varref)
     (* 20948 cost_null)
     (* 15460 cost_funcall)
     (* 1498 cost_constant)
     (* 8977 cost_cons)
     (* 24937 cost_cond)
     (* 13964 cost_cdr)
     (* 16955 cost_car)
     (* 3989 cost_booleanop)))

(print
  'mergesort
  1000
  (+ (* 124710 cost_varref)
     (* 45900 cost_null)
     (* 33924 cost_funcall)
     (* 2998 cost_constant)
     (* 19953 cost_cons)
     (* 54877 cost_cond)
     (* 30928 cost_cdr)
     (* 37907 cost_car)
     (* 8977 cost_booleanop)))

(print
  'mergesort
  2000
  (+ (* 273422 cost_varref)
     (* 99804 cost_null)
     (* 73852 cost_funcall)
     (* 5998 cost_constant)
     (* 43905 cost_cons)
     (* 119757 cost_cond)
     (* 67856 cost_cdr)
     (* 83811 cost_car)
     (* 19953 cost_booleanop)))

(print
  'union
  10
  (+ (* 582 cost_varref)
     (* 121 cost_null)
     (* 120 cost_funcall)
     (* 10 cost_constant)
     (* 10 cost_cons)
     (* 231 cost_cond)
     (* 110 cost_cdr)
     (* 120 cost_car)
     (* 100 cost_booleanop)
     (* 10 cost_binding)))

(print
  'union
  20
  (+ (* 2162 cost_varref)
     (* 441 cost_null)
     (* 440 cost_funcall)
     (* 20 cost_constant)
     (* 20 cost_cons)
     (* 861 cost_cond)
     (* 420 cost_cdr)
     (* 440 cost_car)
     (* 400 cost_booleanop)
     (* 20 cost_binding)))

(print
  'union
  50
  (+ (* 12902 cost_varref)
     (* 2601 cost_null)
     (* 2600 cost_funcall)
     (* 50 cost_constant)
     (* 50 cost_cons)
     (* 5151 cost_cond)
     (* 2550 cost_cdr)
     (* 2600 cost_car)
     (* 2500 cost_booleanop)
     (* 50 cost_binding)))

(print
  'union
  100
  (+ (* 50802 cost_varref)
     (* 10201 cost_null)
     (* 10200 cost_funcall)
     (* 100 cost_constant)
     (* 100 cost_cons)
     (* 20301 cost_cond)
     (* 10100 cost_cdr)
     (* 10200 cost_car)
     (* 10000 cost_booleanop)
     (* 100 cost_binding)))

(print
  'union
  200
  (+ (* 201602 cost_varref)
     (* 40401 cost_null)
     (* 40400 cost_funcall)
     (* 200 cost_constant)
     (* 200 cost_cons)
     (* 80601 cost_cond)
     (* 40200 cost_cdr)
     (* 40400 cost_car)
     (* 40000 cost_booleanop)
     (* 200 cost_binding)))

(print
  'union
  300
  (+ (* 452402 cost_varref)
     (* 90601 cost_null)
     (* 90600 cost_funcall)
     (* 300 cost_constant)
     (* 300 cost_cons)
     (* 180901 cost_cond)
     (* 90300 cost_cdr)
     (* 90600 cost_car)
     (* 90000 cost_booleanop)
     (* 300 cost_binding)))

(print
  'union
  500
  (+ (* 1254002 cost_varref)
     (* 251001 cost_null)
     (* 251000 cost_funcall)
     (* 500 cost_constant)
     (* 500 cost_cons)
     (* 501501 cost_cond)
     (* 250500 cost_cdr)
     (* 251000 cost_car)
     (* 250000 cost_booleanop)
     (* 500 cost_binding)))

(print
  'union
  1000
  (+ (* 5008002 cost_varref)
     (* 1002001 cost_null)
     (* 1002000 cost_funcall)
     (* 1000 cost_constant)
     (* 1000 cost_cons)
     (* 2003001 cost_cond)
     (* 1001000 cost_cdr)
     (* 1002000 cost_car)
     (* 1000000 cost_booleanop)
     (* 1000 cost_binding)))

(print
  'union
  2000
  (+ (* 20016002 cost_varref)
     (* 4004001 cost_null)
     (* 4004000 cost_funcall)
     (* 2000 cost_constant)
     (* 2000 cost_cons)
     (* 8006001 cost_cond)
     (* 4002000 cost_cdr)
     (* 4004000 cost_car)
     (* 4000000 cost_booleanop)
     (* 2000 cost_binding)))


(print
  'reverse
  10
  (+ (* 43 cost_varref)
     (* 11 cost_null)
     (* 11 cost_funcall)
     cost_constant
     (* 10 cost_cons)
     (* 11 cost_cond)
     (* 10 cost_cdr)
     (* 10 cost_car)))

(print
  'reverse
  20
  (+ (* 83 cost_varref)
     (* 21 cost_null)
     (* 21 cost_funcall)
     cost_constant
     (* 20 cost_cons)
     (* 21 cost_cond)
     (* 20 cost_cdr)
     (* 20 cost_car)))

(print
  'reverse
  50
  (+ (* 203 cost_varref)
     (* 51 cost_null)
     (* 51 cost_funcall)
     cost_constant
     (* 50 cost_cons)
     (* 51 cost_cond)
     (* 50 cost_cdr)
     (* 50 cost_car)))

(print
  'reverse
  100
  (+ (* 403 cost_varref)
     (* 101 cost_null)
     (* 101 cost_funcall)
     cost_constant
     (* 100 cost_cons)
     (* 101 cost_cond)
     (* 100 cost_cdr)
     (* 100 cost_car)))

(print
  'reverse
  200
  (+ (* 803 cost_varref)
     (* 201 cost_null)
     (* 201 cost_funcall)
     cost_constant
     (* 200 cost_cons)
     (* 201 cost_cond)
     (* 200 cost_cdr)
     (* 200 cost_car)))

(print
  'reverse
  300
  (+ (* 1203 cost_varref)
     (* 301 cost_null)
     (* 301 cost_funcall)
     cost_constant
     (* 300 cost_cons)
     (* 301 cost_cond)
     (* 300 cost_cdr)
     (* 300 cost_car)))

(print
  'reverse
  500
  (+ (* 2003 cost_varref)
     (* 501 cost_null)
     (* 501 cost_funcall)
     cost_constant
     (* 500 cost_cons)
     (* 501 cost_cond)
     (* 500 cost_cdr)
     (* 500 cost_car)))

(print
  'reverse
  1000
  (+ (* 4003 cost_varref)
     (* 1001 cost_null)
     (* 1001 cost_funcall)
     cost_constant
     (* 1000 cost_cons)
     (* 1001 cost_cond)
     (* 1000 cost_cdr)
     (* 1000 cost_car)))

(print
  'reverse
  2000
  (+ (* 8003 cost_varref)
     (* 2001 cost_null)
     (* 2001 cost_funcall)
     cost_constant
     (* 2000 cost_cons)
     (* 2001 cost_cond)
     (* 2000 cost_cdr)
     (* 2000 cost_car)))


(print
  'nreverse
  10
  (+ (* 231 cost_varref)
     (* 66 cost_null)
     (* 65 cost_funcall)
     (* 11 cost_constant)
     (* 55 cost_cons)
     (* 66 cost_cond)
     (* 55 cost_cdr)
     (* 55 cost_car)))

(print
  'nreverse
  20
  (+ (* 861 cost_varref)
     (* 231 cost_null)
     (* 230 cost_funcall)
     (* 21 cost_constant)
     (* 210 cost_cons)
     (* 231 cost_cond)
     (* 210 cost_cdr)
     (* 210 cost_car)))

(print
  'nreverse
  50
  (+ (* 5151 cost_varref)
     (* 1326 cost_null)
     (* 1325 cost_funcall)
     (* 51 cost_constant)
     (* 1275 cost_cons)
     (* 1326 cost_cond)
     (* 1275 cost_cdr)
     (* 1275 cost_car)))

(print
  'nreverse
  100
  (+ (* 20301 cost_varref)
     (* 5151 cost_null)
     (* 5150 cost_funcall)
     (* 101 cost_constant)
     (* 5050 cost_cons)
     (* 5151 cost_cond)
     (* 5050 cost_cdr)
     (* 5050 cost_car)))

(print
  'nreverse
  200
  (+ (* 80601 cost_varref)
     (* 20301 cost_null)
     (* 20300 cost_funcall)
     (* 201 cost_constant)
     (* 20100 cost_cons)
     (* 20301 cost_cond)
     (* 20100 cost_cdr)
     (* 20100 cost_car)))

(print
  'nreverse
  300
  (+ (* 180901 cost_varref)
     (* 45451 cost_null)
     (* 45450 cost_funcall)
     (* 301 cost_constant)
     (* 45150 cost_cons)
     (* 45451 cost_cond)
     (* 45150 cost_cdr)
     (* 45150 cost_car)))

(print
  'nreverse
  500
  (+ (* 501501 cost_varref)
     (* 125751 cost_null)
     (* 125750 cost_funcall)
     (* 501 cost_constant)
     (* 125250 cost_cons)
     (* 125751 cost_cond)
     (* 125250 cost_cdr)
     (* 125250 cost_car)))

(print
  'nreverse
  1000
  (+ (* 2003001 cost_varref)
     (* 501501 cost_null)
     (* 501500 cost_funcall)
     (* 1001 cost_constant)
     (* 500500 cost_cons)
     (* 501501 cost_cond)
     (* 500500 cost_cdr)
     (* 500500 cost_car)))

(print
  'nreverse
  2000
  (+ (* 8006001 cost_varref)
     (* 2003001 cost_null)
     (* 2003000 cost_funcall)
     (* 2001 cost_constant)
     (* 2001000 cost_cons)
     (* 2003001 cost_cond)
     (* 2001000 cost_cdr)
     (* 2001000 cost_car)))


(print
  'product
  10
  (+ (* 981 cost_varref)
     (* 231 cost_null)
     (* 230 cost_funcall)
     (* 111 cost_constant)
     (* 400 cost_cons)
     (* 231 cost_cond)
     (* 210 cost_cdr)
     (* 210 cost_car)))

(print
  'product
  20
  (+ (* 3761 cost_varref)
     (* 861 cost_null)
     (* 860 cost_funcall)
     (* 421 cost_constant)
     (* 1600 cost_cons)
     (* 861 cost_cond)
     (* 820 cost_cdr)
     (* 820 cost_car)))

(print
  'product
  50
  (+ (* 22901 cost_varref)
     (* 5151 cost_null)
     (* 5150 cost_funcall)
     (* 2551 cost_constant)
     (* 10000 cost_cons)
     (* 5151 cost_cond)
     (* 5050 cost_cdr)
     (* 5050 cost_car)))

(print
  'product
  100
  (+ (* 90801 cost_varref)
     (* 20301 cost_null)
     (* 20300 cost_funcall)
     (* 10101 cost_constant)
     (* 40000 cost_cons)
     (* 20301 cost_cond)
     (* 20100 cost_cdr)
     (* 20100 cost_car)))

(print
  'product
  200
  (+ (* 361601 cost_varref)
     (* 80601 cost_null)
     (* 80600 cost_funcall)
     (* 40201 cost_constant)
     (* 160000 cost_cons)
     (* 80601 cost_cond)
     (* 80200 cost_cdr)
     (* 80200 cost_car)))

(print
  'product
  300
  (+ (* 812401 cost_varref)
     (* 180901 cost_null)
     (* 180900 cost_funcall)
     (* 90301 cost_constant)
     (* 360000 cost_cons)
     (* 180901 cost_cond)
     (* 180300 cost_cdr)
     (* 180300 cost_car)))

(print
  'product
  500
  (+ (* 2254001 cost_varref)
     (* 501501 cost_null)
     (* 501500 cost_funcall)
     (* 250501 cost_constant)
     (* 1000000 cost_cons)
     (* 501501 cost_cond)
     (* 500500 cost_cdr)
     (* 500500 cost_car)))

(print
  'product
  1000
  (+ (* 9008001 cost_varref)
     (* 2003001 cost_null)
     (* 2003000 cost_funcall)
     (* 1001001 cost_constant)
     (* 4000000 cost_cons)
     (* 2003001 cost_cond)
     (* 2001000 cost_cdr)
     (* 2001000 cost_car)))

(print
  'product
  2000
  (+ (* 36016001 cost_varref)
     (* 8006001 cost_null)
     (* 8006000 cost_funcall)
     (* 4002001 cost_constant)
     (* 16000000 cost_cons)
     (* 8006001 cost_cond)
     (* 8002000 cost_cdr)
     (* 8002000 cost_car)))
