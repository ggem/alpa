;;
;;
;;

(define tests
  '((ack (3 1) (3 5) (3 7) (3 9))
    (ackermann (3 1) (3 5) (3 7) (3 9))
    (cpstak (19 8 1) (19 9 1) (19 9 3) (19 10 1))
    (fix (10) (20) (50) (100) (200) (500) (1000) (2000))
    (index (10) (20) (50) (100) (200) (500) (1000) (2000))
    (lattice ())
    (map (10) (20) (50) (100) (200) (500) (1000) (2000))
    (reverse (10) (20) (50) (100) (200) (500) (1000) (2000))
    (reverse-cps (10) (20) (50) (100) (200) (500) (1000) (2000))
    (split (10) (20) (50) (100) (200) (500) (1000) (2000))
    (union (10 10) (20 20) (50 50) (100 100)
      (200 200) (500 500) (1000 1000) (2000 2000))))

(define calls
  '(;(ack 3 1)
    ;(ack 3 5)
    ;(ack 3 7)
    ;(ack 3 9)
    ;(ackermann 3 1)
    ;(ackermann 3 5)
    ;(ackermann 3 7)
    ;(ackermann 3 9)
    ;(cpstak 19 8 1)
    ;(cpstak 19 9 1)
    ;(cpstak 19 9 3)
    ;(cpstak 19 10 1)
    ;((fix sum) 10)
    ;((fix sum) 20)
    ;((fix sum) 50)
    ;((fix sum) 100)
    ;((fix sum) 200)
    ;((fix sum) 500)
    ;((fix sum) 1000)
    ;((fix sum) 2000)
    (index 1 list-10)
    (index 1 list-20)
    (index 1 list-50)
    (index 1 list-100)
    (index 1 list-200)
    (index 1 list-500)
    (index 1 list-1000)
    (index 1 list-2000)
    ;(lattice)
    ;(map (c-add 4) list-10)
    ;(map (c-add 4) list-20)
    ;(map (c-add 4) list-50)
    ;(map (c-add 4) list-100)
    ;(map (c-add 4) list-200)
    ;(map (c-add 4) list-500)
    ;(map (c-add 4) list-1000)
    ;(map (c-add 4) list-2000)
    ;(reverse list-10)
    ;(reverse list-20)
    ;(reverse list-50)
    ;(reverse list-100)
    ;(reverse list-200)
    ;(reverse list-500)
    ;(reverse list-1000)
    ;(reverse list-2000)
    ;(reverse-cps list-10)
    ;(reverse-cps list-20)
    ;(reverse-cps list-50)
    ;(reverse-cps list-100)
    ;(reverse-cps list-200)
    ;(reverse-cps list-500)
    ;(reverse-cps list-1000)
    ;(reverse-cps list-2000)
    ;(split list-10 interval-tester initial-k)
    ;(split list-20 interval-tester initial-k)
    ;(split list-50 interval-tester initial-k)
    ;(split list-100 interval-tester initial-k)
    ;(split list-200 interval-tester initial-k)
    ;(split list-500 interval-tester initial-k)
    ;(split list-1000 interval-tester initial-k)
    ;(split list-2000 interval-tester initial-k)
    ;(union list-10 listb-10)
    ;(union list-20 listb-20)
    ;(union list-50 listb-50)
    ;(union list-100 listb-100)
    ;(union list-200 listb-200)
    ;(union list-500 listb-500)
    ;(union list-1000 listb-1000)
    ;(union list-2000 listb-2000)
    ))

(define start-up-code
  '(begin
     (define list-10    (append! (make-list 9    314159)  (list 1)))
     (define list-20    (append! (make-list 19   314159)  (list 1)))
     (define list-50    (append! (make-list 49   314159)  (list 1)))
     (define list-100   (append! (make-list 99   314159)  (list 1)))
     (define list-200   (append! (make-list 199  314159)  (list 1)))
     (define list-500   (append! (make-list 499  314159)  (list 1)))
     (define list-1000  (append! (make-list 999  314159)  (list 1)))
     (define list-2000  (append! (make-list 1999 314159)  (list 1)))
     (define listb-10   (append! (make-list 9    3141593) (list 2)))
     (define listb-20   (append! (make-list 19   3141593) (list 2)))
     (define listb-50   (append! (make-list 49   3141593) (list 2)))
     (define listb-100  (append! (make-list 99   3141593) (list 2)))
     (define listb-200  (append! (make-list 199  3141593) (list 2)))
     (define listb-500  (append! (make-list 499  3141593) (list 2)))
     (define listb-1000 (append! (make-list 999  3141593) (list 2)))
     (define listb-2000 (append! (make-list 1999 3141593) (list 2)))
     (define my-print (lambda (fmt . args)
			'(with-output-to-file "/dev/pts/38"
			  (lambda () (apply printf fmt args))
			  'append)
			(if (null? args) fmt (car args))))
     (define time-exp
       (let ([min-time 15000])		; 2 minutes
	 (lambda (thunk)
	   (let ([timer
		   (lambda (loops)
		     (let ([stt1 (statistics)])
		       (do ([i loops (- i 1)])
			 [(= i 0) 0]
			 (thunk))
		       (let ([stt2 (statistics)])
			 (let ([cpu-time1 (vector-ref stt1 1)]
			       [gc-time1  (vector-ref stt1 5)]
			       [cpu-time2 (vector-ref stt2 1)]
			       [gc-time2  (vector-ref stt2 5)])
			   (- (- cpu-time2 gc-time2)
			     (- cpu-time1 gc-time1))))))])
	     (do ([loops 1 (my-print "~a ~a ~a~%"
			     (ceiling
			       (* (min 100 (/ min-time (+ 1 timed))) loops))
			     timed loops)]
		  [timed (timer 1) (timer loops)]
		  [old-loops 1 loops])
	       [(> timed min-time) (/ timed old-loops 1.0)]
	       (my-print "~a:~a:~a	~a~%" old-loops timed
		 (+ 0.0 (/ timed old-loops)) loops)
	       )))))))

(for-each
  (lambda (call)
    (printf "~a ==> " call)
    (flush-output-port (current-output-port))
    (let* ([file (if (symbol? (car call)) (car call) (caar call))]
	   [scheme (process "scheme6")]
	   [ip (car scheme)]
	   [op (cadr scheme)])
      (write start-up-code op)
      (fprintf op "(begin
		     (load \"/u/ggomezes/alpa/closure/!tests/~a.ss\")
		     (printf \"ignore-up-to-here~~%\")
		     (display (time-exp (lambda () ~a)))
		     (newline)
		     (exit))"
	file call)
      (flush-output-port op)
      '(do ([ch (read-char ip) (read-char ip)])
	[(eof-object? ch)]
	(write-char ch))
      (do ([x (read ip) (read ip)])
	[(eq? x 'ignore-up-to-here) (printf "~a milliseconds.~%" (read ip))]
	'(pretty-print x))
      (close-output-port op)
      (close-input-port ip)))
  calls)

(exit)

;;;
;;; old code.  To return the cost functions for each procedure.
;;;

'(load "~/alpa/closure/alpa.ss")
'(load "~/alpa/closure/!timing/cost-functions.ss")

'(for-each
  (lambda (line)
    (let ([proc-name (car line)]
	  [args-list (cdr line)])
      (let ([proc (eval (string->symbol (format "cost-~a" proc-name)))])
	(do ([args-list args-list (cdr args-list)])
	  [(null? args-list) #f]
	  (printf "(cost-~a~%   ~a" proc-name (car args-list))
	  (pretty-print (apply proc (car args-list)))
	  (printf ")~%")))))
  files)
