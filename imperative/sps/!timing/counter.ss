;;;
;;; counter.  interpreter that counts each of the primitive operations
;;;

(define iota
  (letrec ([iota (lambda (n acc)
                   (if (< n 0)
                       acc
                       (iota (- n 1) (cons n acc))))])
    (lambda (n)
      (iota (- n 1) '()))))

(define primitives
  '((cons		. (x y))
    (car		. (x))
    (cdr		. (x))
    (set-car!		. (x y))
    (set-cdr!		. (x y))
    (null?		. (x))
    (make-vector	. (x y))
    (vector		. #f)
    (vector-ref		. (x y))
    (vector-length	. (x))
    (vector-set!	. (x y z))
    (eq?		. (x y))
    (+			. (x y))
    (-			. (x y))
    (*			. (x y))
    (quotient		. (x y))
    (>			. (x y))
    (<			. (x y))
    (=			. (x y))))

(define costs-names
  (map (lambda (name) (string->symbol (format "cost_~a" name)))
    '(cons car cdr set-car! set-cdr! null?
       make-vector vector vector-ref vector-length vector-set!
       eq?  + - * quotient > < =
       const varref if let funcall set! while)))

(define-syntax while
  (syntax-rules ()
    ((while test body0 body1 ...)
     (let loop ()
       (when test
         body0 body1 ... (loop))))))

(define count
  (let ([make-primitive-counter
          (lambda (sym args idx)
            (if args
                `(,sym (lambda ,args (inc ,idx) (,sym . ,args)))
                `(,sym (lambda args (inc ,idx) (apply ,sym args)))))]
        [offset (length primitives)]
        [prims (map car primitives)]
        [args (map cdr primitives)])
    (let ([primitive-counters
            (map make-primitive-counter prims args (iota offset))])
      (letrec ([make-counter
                 (lambda (exp)
                   (cond
                     [(symbol? exp) `(begin (inc ,(+ offset 1)) ,exp)]
                     [(null? exp) `(begin (inc ,offset) ,exp)]
                     [(number? exp) `(begin (inc ,offset) ,exp)]
                     [(boolean? exp) `(begin (inc ,offset) ,exp)]
                     [(atom? exp) (error 'count "unknown type: ~s" exp)]
                     [(eq? (car exp) 'define)
                      (if #f
                          `(trace-define ,(caadr exp)
                             (lambda ,(cdadr exp) ,(make-counter (caddr exp))))
                          `(define ,(cadr exp) ,(make-counter (caddr exp))))]
                     [(eq? (car exp) 'quote) `(begin (inc ,offset) ,exp)]
                     [(eq? (car exp) 'begin) `(begin . ,(map make-counter (cdr exp)))]
                     [(eq? (car exp) 'if)
                      `(begin
                         (inc ,(+ offset 2))
                         (if . ,(map make-counter (cdr exp))))]
                     [(eq? (car exp) 'set!)
                      `(begin
                         (inc ,(+ offset 5))
                         (set! ,(cadr exp) ,(make-counter (caddr exp))))]
                     [(eq? (car exp) 'while)
                      `(begin
                         (inc ,(+ offset 6))
                         (while . ,(map make-counter (cdr exp))))]
                     [(eq? (car exp) 'let)
                      `(let ,(map (lambda (binding)
                                    (list (car binding)
                                      (make-counter (cadr binding))))
                               (cadr exp))
                         (inc ,(+ offset 3)) . ,(map make-counter (cddr exp)))]
                     [(memq (car exp) prims)
                      `(,(car exp) . ,(map make-counter (cdr exp)))]
                     [else
                       `(begin (inc ,(+ offset 4))
                               (,(car exp) . ,(map make-counter (cdr exp))))]))])
        (lambda (definitions call)
          (let ([expr
                  `(let ([counts (#%make-vector ,(length costs-names) 0)])
                     (let ([inc (lambda (i)
                                  (vector-set! counts i
                                    (+ 1 (vector-ref counts i))))])
                       (let ,primitive-counters
                         ,@(map make-counter definitions)
                         ,call
                         (#%cons '+
                           (let loop ([counts (vector->list counts)]
                                      [names costs-names]
                                      [answ '()])
                             (if (null? counts)
                                 answ
                                 (loop (cdr counts) (cdr names)
                                   (let ([count (car counts)])
                                     (cond
                                       [(> count 1)
                                        `((* ,count ,(car names)) . ,answ)]
                                       [(= count 1)
                                        `(,(car names) . ,answ)]
                                       [else answ])))))))))])
            ;;(pretty-print expr)
            (eval expr)))))))


(define size 8)

(define make-vector-unknowns
  (lambda (kind)
    (case kind
      [(mergesort)
       (let ([v (make-vector size #f)])
         (vector-set! v (- size 1) (- size 1))
         (let loop ([start 0] [size size] [n (- size 2)])
           (if (> size 1)
               (let ([mid (quotient size 2)])
                 (vector-set! v (+ start mid -1) n)
                 (loop start mid (- n 1))
                 (loop (+ start mid) (- size mid) (- n mid)))))
         v)]
      [(iota) (list->vector (iota size))]
      [(reverse-iota) (list->vector (reverse (iota size)))]
      [else (make-vector size kind)])))

(define make-list-unknowns
  (lambda (kind)
    (case kind
      [(mergesort)
       (let ([v (make-vector size #f)])
         (vector-set! v (- size 1) (- size 1))
         (let loop ([start 0] [size size] [n (- size 2)])
           (if (> size 1)
               (let ([mid (quotient size 2)])
                 (vector-set! v (+ start mid -1) n)
                 (loop start mid (- n 1))
                 (loop (+ start mid) (- size mid) (- n mid)))))
         (vector->list v))]
      [(iota) (iota size)]
      [(reverse-iota) (reverse (iota size))]
      [else (make-list size kind)])))


(define count-testfile
  (lambda (filename . sizes)
    (with-input-from-file filename
      (lambda ()
        (let ([x (read)])
          (if (eof-object? x)
              (error 'count-testfile "invalid empty file: ~s" filename)
              (let loop ([last x] [ls '()])
                (let ([x (read)])
                  (if (eof-object? x)
                      (do ([sizes sizes (cdr sizes)])
                          [(null? sizes) 'done]
                        (fluid-let ([size (car sizes)])
                          (printf "; ~a: size: ~a~%" filename size)
                          (pretty-print (count ls (cadr last)))))
                      (loop x (cons last ls)))))))))))
