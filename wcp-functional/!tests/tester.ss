;
;

(define dir "/u/ggomezes/alpa/wcp-functional")

(define test-cases
  '(
    ;(big-functions 1)
    ;(bottom-up-mergesort 1)
    ;(index 1)
    (insertsort 1)
    ;(lcs 1)
    ;(mergesort 1)
    ;(odd-even-mergesort 1)
    ;(selectsort 1)
    (union 2)
    ))

(define (test-translation-nooptimized-time)
  (define inner-loop 100)
  (define outer-loop 5)
  (current-directory dir)
  (for-each
    (lambda (filename)
      (let ([code (read-file (format "!tests/~a.ss" filename))])
        (let loop ([i outer-loop] [count 0] [sum 0])
          (if (zero? i)
              (printf "~a: ~a ms~%" filename (+ 1.0 (/ sum count inner-loop)))
              (let ([output
                      (run-scheme-and-list
                        `(begin
                           (current-directory ,dir)
                           (load "!backups/wcp.03.ss")
                           (define optimize append)
                           (let ([s1 (statistics)])
                             (do ([i ,inner-loop (- i 1)])
                                 [(zero? i) 'done]
                               (wcp ',code))
                             (let ([s2 (statistics)])
                               (display
                                 (sstats-cpu (sstats-difference s2 s1)))))))])
                (if (and (pair? output)
                         (null? (cdr output))
                         (number? (car output)))
                    (loop (- i 1) (+ count 1) (+ sum (car output)))
                    (loop (- i 1) count sum)))))))
    (map car test-cases)))

(define (test-translation-optimized-time)
  (define inner-loop 100)
  (define outer-loop 5)
  (current-directory dir)
  (for-each
    (lambda (filename)
      (let ([code (read-file (format "!tests/~a.ss" filename))])
        (let loop ([i outer-loop] [count 0] [sum 0])
          (if (zero? i)
              (printf "~a: ~a ms~%" filename (+ 1.0 (/ sum count inner-loop)))
              (let ([output
                      (run-scheme-and-list
                        `(begin
                           (current-directory ,dir)
                           (load "wcp.ss")
                           (let ([s1 (statistics)])
                             (do ([i ,inner-loop (- i 1)])
                                 [(zero? i) 'done]
                               (wcp ',code))
                             (let ([s2 (statistics)])
                               (display
                                 (sstats-cpu (sstats-difference s2 s1)))))))])
                (if (and (pair? output)
                         (null? (cdr output))
                         (number? (car output)))
                    (loop (- i 1) (+ count 1) (+ sum (car output)))
                    (loop (- i 1) count sum)))))))
    (map car test-cases)))


(define (test-wcp-nooptimized-time)
  (define inner-loop 1)
  (define outer-loop 10)
  (current-directory dir)
  (load "!backups/wcp.03.ss")
  (load "utils.ss")
  (set! optimize (lambda (x) x))	; disable optimizations
  (for-each
   (lambda (size)
     (for-each
      (lambda (testcase)
	(let* ([filename (format "!tests/~a.ss" (car testcase))]
	       [code (scheme-wrap (wcp-file filename))]
	       [timeproc (cadr code)]
	       [proc-call (cons timeproc (make-list (cadr testcase) size))])
	  (let loop ([i outer-loop] [count 0] [sum 0.0])
	    (if (zero? i)
		(printf "~a: ~a ms~%" proc-call (/ sum count inner-loop))
		(let ([output
		       (run-scheme-and-list
			`(begin
			   (optimize-level 3)
			   ,code
			   (define Tcar 87.45)
			   (define Tcdr 60.95)
			   (define Tcons 95.8)
			   (define Tnull? 73.3)
			   (define Teq? 66.65)
			   (define T+ 66.55)
			   (define T- 65.15)
			   (define T* 509.45)
			   (define Tquotient 300.00) ; made up number!
			   (define T> 80.4)
			   (define T< 75.25)
			   (define T= 75.55)
			   (define Tc 0.25)
			   (define Tvar 2.1)
			   (define Tif 3.7)
			   (define Tlet 0.45)
			   (define Tcall 75.175)
			   (define flatten-path
			     (letrec ([flatten
				       (lambda (path acc)
					 (cond
					   [(null? path) acc]
					   [(null? (car path))
					    (flatten (cdr path) acc)]
					   [(symbol? (caar path))
					    (cons (car path) (flatten (cdr path) acc))]
					   [else
					    (flatten (car path) (flatten (cdr path) acc))]))])
			       (lambda (path)
				 (flatten path '()))))
			   (let ([s1 (sstats-cpu (statistics))])
			     (do ([i ,inner-loop (- i 1)])
				 [(zero? i) 'done]
			       ,proc-call)
			     (let ([s2 (sstats-cpu (statistics))])
			       (printf "~a~%" (- s2 s1))))))])
		  (printf "; ~a~%" output)
		  (if (and (pair? output)
			   (null? (cdr output))
			   (number? (car output)))
		      (loop (- i 1) (+ count 1) (+ sum (car output)))
		      (loop (- i 1) count sum)))))))
      test-cases))
   '(500 1000)))


(define (test-wcp-optimized-time)
  (define inner-loop 1)
  (define outer-loop 10)
  (current-directory dir)
  (load "wcp.ss")
  (for-each
   (lambda (size)
     (for-each
      (lambda (testcase)
	(let* ([filename (format "!tests/~a.ss" (car testcase))]
	       [code (scheme-wrap (wcp-file filename))]
	       [timeproc (cadr code)]
	       [proc-call (cons timeproc (make-list (cadr testcase) size))])
	  (let loop ([i outer-loop] [count 0] [sum 0.0])
	    (if (zero? i)
		(if (zero? count)
		    (printf "~a: ~a ms~%" proc-call '--)
		    (printf "~a: ~a ms~%" proc-call (/ sum count inner-loop)))
		(let ([output
		       (run-scheme-and-list
			`(begin
			   (optimize-level 3)
			   ,code
			   (define flatten-path
			     (letrec ([flatten
				       (lambda (path acc)
					 (cond
					   [(null? path) acc]
					   [(null? (car path))
					    (flatten (cdr path) acc)]
					   [(symbol? (caar path))
					    (cons (car path) (flatten (cdr path) acc))]
					   [else
					    (flatten (car path) (flatten (cdr path) acc))]))])
			       (lambda (path)
				 (flatten path '()))))
			   (let ([s1 (sstats-cpu (statistics))])
			     (do ([i ,inner-loop (- i 1)])
				 [(zero? i) 'done]
			       ,proc-call)
			     (let ([s2 (sstats-cpu (statistics))])
			       (printf "~a~%" (- s2 s1))))))])
		  (printf "; ~a~%" output)
		  (if (and (pair? output)
			   (null? (cdr output))
			   (number? (car output)))
		      (loop (- i 1) (+ count 1) (+ sum (car output)))
		      (loop (- i 1) count sum)))))))
      test-cases))
   '(1000)))


; index.ss: 		  3.72 ms	  9 lines
; insertsort.ss: 	  5.52 ms	 12 lines
; union.ss: 		  5.70 ms	 14 lines
; selectsort.ss: 	  7.56 ms	 16 lines
; odd-even-mergesort:	  9.84 ms	 23 lines
; bottom-up-mergesort:	 11.96 ms	 31 lines
; merge-sort.ss:	 15.40 ms	 34 lines
; lcs-incr.ss:		 18.60 ms	 34 lines
; big-functions.ss	526.70 ms	700 lines
