;

(define procs '((insertsort . "insert sort")
                (list-mergesort . "merge sort (list)")
                (mergesort . "merge sort (vector)")
                (reverse! . "reverse")
                (selectsort . "select sort")
                (vector-sum . "vector sum")))

(define kinds '(measured computed computed2))

(define make-plot-files
  (lambda ()
    (do ([kinds kinds (cdr kinds)])
        [(null? kinds) 'done]
      (do ([procs procs (cdr procs)])
          [(null? procs) 'done]
        (let ([kind (car kinds)]
              [proc (caar procs)])
          (let ([command (format
                           (string-append
                             "sed -n 's/(~a[ \t]*\\([0-9]*\\)[ \t]*\\([0-9.]*\\))/\\1 \\2/p'"
                             " < ../\\!timing/~a.txt"
                             " > ~a.~a.txt")
                            proc kind proc kind)])
            (system command)))))))

(define (make-plots)
  (let ([pr (process "gnuplot")])
    (let ([gnuplot
            (cadr pr)
            ;(current-output-port)
            ])
      (display "set xlabel \"input size\" \"Times\"\n" gnuplot)
      (display "set ylabel \"time (millisecconds)\" \"Times\"\n" gnuplot)
      (display "set data style linespoints\n" gnuplot)
      (display "set size 0.5,0.5\n" gnuplot)
      (display "set terminal postscript eps\n" gnuplot)
      (do ([procs procs (cdr procs)])
          [(null? procs)
           (close-input-port (car pr))
           (close-output-port (cadr pr))]
        (let ([name (caar procs)]
              [title (cdar procs)])
          (fprintf gnuplot "set title ~s \"Times\"\n" title)
          (fprintf gnuplot "set output \"~a-sps.eps\"\n" name)
          (fprintf gnuplot "plot [0:1000] \"~a.measured.txt\" t \"measured\" , \"~a.computed.txt\" t \"computed\"\n"
            name name)
          (fprintf gnuplot "set output \"~a-direct.eps\"\n" name)
          (fprintf gnuplot "plot [0:1000] \"~a.measured.txt\" t \"measured\" , \"~a.computed2.txt\" t \"computed (direct)\" , \"~a.computed.txt\" t \"computed (sps)\"\n"
            name name name))))))

(make-plots)