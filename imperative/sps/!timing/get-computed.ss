;

(load "primitives.log")

(define file->list
  (let ([read-current-port (lambda ()
                             (do ([ls '() (cons obj ls)]
                                  [obj (read) (read)])
                                 [(eof-object? obj) (reverse ls)]))])
    (lambda (filename)
      (with-input-from-file filename read-current-port))))

(define (main input output)
  (with-input-from-file input
    (lambda ()
      (with-output-to-file output
        (lambda ()
          (do ([entry (read) (read)])
              [(eof-object? entry) 'done]
            (printf "(~a	~a	~a)\n"
              (caar entry)
              (cadar entry)
              (eval (caddr entry)))))
        'replace))))


' (main "alpaed.log" "computed.txt")
' (main "counter.log" "computed2.txt")

(define sizes '#(10 20 50 100 200 300 500 1000 2000))
(define procs '#(insertsort list-mergesort mergesort
                  reverse! selectsort vector-sum))

(define list->table
  (lambda (lst)
    (let ([table (make-vector (vector-length procs))])
      (let loop ([i 0] [lst lst])
        (if (= i (vector-length procs))
            table
            (let ([row (make-vector (vector-length sizes))])
              (vector-set! table i row)
              (do ([j 0 (+ j 1)]
                   [lst lst (cdr lst)])
                  [(= j (vector-length sizes)) (loop (+ i 1) lst)]
                (vector-set! row j (caddar lst)))))))))

(define measured  (list->table (file->list "measured.txt")))
(define computed  (list->table (file->list "computed.txt")))
(define computed2 (list->table (file->list "computed2.txt")))


(define make-latex-table
  (let ([header "\\begin{tabular}{|@{\\,}r@{\\,}||@{\\,}r@{\\,}|@{\\,}r@{\\,}|@{\\,}r@{\\,}||@{\\,}r@{\\,}|@{\\,}r@{\\,}|@{\\,}r@{\\,}||@{\\,}r@{\\,}|@{\\,}r@{\\,}|@{\\,}r@{\\,}|}\\hline\n  & \\multicolumn{3}{@{\\,}c@{\\,}||@{\\,}}{~a}\n  & \\multicolumn{3}{@{\\,}c@{\\,}||@{\\,}}{~a}\n  & \\multicolumn{3}{@{\\,}c@{\\,}|}{~a}\\\\\\cline{2-10}\n  size &{calculated}&{measured}&{me/ca}\n       &{calculated}&{measured}&{me/ca}\n       &{calculated}&{measured}&{me/ca}\\\\\\hline\n"]
        [footer "\\hline\n\\end{tabular}\n"]
        [make-entry
          (lambda (proc size)
            (let ([comp (vector-ref (vector-ref computed proc) size)]
                  [meas (vector-ref (vector-ref measured proc) size)])
              (printf "\t&~a\t&~a\t&~a" comp meas (* 100.0 (/ meas comp)))))])
    (lambda (a b c)
      (printf header
        (vector-ref procs a) (vector-ref procs b) (vector-ref procs c))
      (do ([size 0 (+ size 1)])
          [(= size (vector-length sizes))
           (printf footer)]
        (printf "~a" (vector-ref sizes size))
        (make-entry a size)
        (make-entry b size)
        (make-entry c size)
        (printf "\t\\\\\n")))))

(define make-latex-tables
  (let ([header "\\begin{tabular}{@{}l@{}}\n"]
        [middle "\\\\\\\\\n"]
        [footer "\\end{tabular}\n"])
    (let ([make-table
            (lambda ()
              (display header)
              (make-latex-table 0 1 2)
              (display middle)
              (make-latex-table 3 4 5)
              (display footer))])
      (lambda ()
        (make-table)
        (fluid-let ([computed computed2])
          (make-table))))))
