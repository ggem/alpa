; This is a heavily beefed up version of an example from the
; Abelson/Sussman textbook. I included some more rules for
; taking derivatives and a lot of expression simplification.
; Also I added the possibility to handle expression in prefix,
; infix & postfix notation (default is infix).

; usage : the first parameter is the expression, the second the variable
; - known functions are : sin, cos, arctan, sqrt
; - results are simplified up to a certain degree

; (derive-x n f) calculates the n-th derivative of f where x is the variable
; (derivations-x function start end) prints all derivatives up to the n-th

; example session
;         (deriv '((3 * x) + (5 * (x ** 7))) 'x)
; (3 + (35 * (x ** 6)))
;         (deriv '((3 * x) + (5 * (x ** 7))) 'y)
; 0
;         (define order 'prefix)
; order
;         (deriv '(+ (* 3 x) (* 5 (** x 7))) 'x)
; (+ 3 (* 35 (** x 6)))

; Comments, questions, bug reports etc. to
;       Ulf Dittmer       ucdittme@top.cis.syr.edu
 
(define order 'infix)

(define (deriv exp var)
  (cond ((constant? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (firstop exp) var)
                   (deriv (secondop exp) var)))
        ((difference? exp)
         (make-difference (deriv (firstop exp) var)
                          (deriv (secondop exp) var)))
        ((product? exp)
         (make-sum
          (make-product (firstop exp)
                        (deriv (secondop exp) var))
          (make-product (deriv (firstop exp) var)
                        (secondop exp))))
        ((quotient? exp)
         (make-quotient (make-difference
                         (make-product (secondop exp)
                                       (deriv (firstop exp) var))
                         (make-product (firstop exp)
                                       (deriv (secondop exp) var)))
                        (make-exponentiation (secondop exp) 2)))
        ((exponentiation? exp)
         (if (equal? 0 (deriv (secondop exp) var))
             (make-product
              (deriv (firstop exp) var)
              (make-product (secondop exp)
                            (make-exponentiation
                             (firstop exp)
                             (make-difference (secondop exp) 1))))
             (make-product exp
                           (make-sum
                            (make-product (deriv (secondop exp) var)
                                          (make-logarithm 'e (firstop exp)))
                            (make-product (deriv (firstop exp) var)
                                          (make-quotient (secondop exp)
                                                         (firstop exp)))))))
        ((function? exp)
         (case (ffunc exp)
           ('sin (make-product (deriv (fop exp) var)
                               (make-function 'cos (fop exp))))
           ('cos (make-product (make-product -1 (deriv (fop exp) var))
                               (make-function 'sin (fop exp))))
           ('sqrt (make-quotient (make-product 0.5 (deriv (fop exp) var))
                                 (make-function 'sqrt (fop exp))))
           ('arctan (make-quotient
                     (deriv (fop exp) var)
                     (make-sum 1 (make-exponentiation (fop exp) 2))))))
        ((logarithm? exp)
         (make-product
          (deriv (secondop exp) var)
          (make-quotient (make-logarithm (firstop exp) 'e) 'x)))
        (else #f)))

(define (constant? x) (number? x))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) (eq? v1 v2))

(define (make-sum a1 a2)
  (let ((worstcase (make-worstcase a1 '+ a2)))
    (cond ((and (number? a1) (number? a2)) (+ a1 a2))
          ((equal? a1 a2) (make-product 2 a1))
          ((number? a1) (cond ((= a1 0) a2)
                              ((sum? a2)
                               (cond ((number? (firstop a2))
                                      (make-sum
                                       (+ a1 (firstop a2)) (secondop a2)))
                                     ((number? (secondop a2))
                                      (make-sum
                                       (+ a1 (secondop a2)) (firstop a2)))
                                     (else worstcase)))
                              ((difference? a2)
                               (cond ((number? (firstop a2))
                                      (make-difference
                                       (+ a1 (firstop a2)) (secondop a2)))
                                     ((number? (secondop a2))
                                      (make-sum
                                       (- a1 (secondop a2)) (firstop a2)))
                                     (else worstcase)))
                              (else worstcase)))
          ((number? a2) (cond ((= a2 0) a1)
                              ((sum? a1)
                               (cond ((number? (firstop a1))
                                      (make-sum
                                       (+ a2 (firstop a1)) (secondop a1)))
                                     ((number? (secondop a1))
                                      (make-sum
                                       (+ a2 (secondop a1)) (firstop a1)))
                                     (else worstcase)))
                              ((difference? a1)
                               (cond ((number? (firstop a1))
                                      (make-difference
                                       (+ a2 (firstop a1)) (secondop a1)))
                                     ((number? (secondop a1))
                                      (make-difference
                                       (firstop a1) (- (secondop a1) a2)))
                                     (else worstcase)))
                              (else worstcase)))
          (else worstcase))))

(define (make-difference a1 a2)
  (let ((worstcase (make-worstcase a1 '- a2)))
    (cond ((and (number? a1) (number? a2)) (- a1 a2))
          ((equal? a1 a2) 0)
          ((number? a1) (cond ((= a1 0) (make-product -1 a2))
                              ((sum? a2)
                               (cond ((number? (firstop a2))
                                      (make-difference
                                       (- a1 (firstop a2)) (secondop a2)))
                                     ((number? (secondop a2))
                                      (make-difference
                                       (- a1 (secondop a2)) (firstop a2)))
                                     (else worstcase)))
                              ((difference? a2)
                               (cond ((number? (firstop a2))
                                      (make-sum
                                       (- a1 (firstop a2)) (secondop a2)))
                                     ((number? (secondop a2))
                                      (make-difference
                                       (+ a1 (secondop a2)) (firstop a2)))
                                     (else worstcase)))
                              (else worstcase)))
          ((number? a2) (cond ((= a2 0) a1)
                              ((sum? a1)
                               (cond ((number? (firstop a1))
                                      (make-sum
                                       (- (firstop a1) a2) (secondop a1)))
                                     ((number? (secondop a1))
                                      (make-sum
                                       (- (secondop a1) a2) (firstop a1)))
                                     (else worstcase)))
                              ((difference? a1)
                               (cond ((number? (firstop a1))
                                      (make-difference
                                       (- (firstop a1) a2) (secondop a1)))
                                     ((number? (secondop a1))
                                      (make-difference
                                       (firstop a1) (+ (secondop a1) a2)))
                                     (else worstcase)))
                              (else worstcase)))
          (else worstcase))))

(define (make-product m1 m2)
  (let ((worstcase (make-worstcase m1 '* m2)))
    (cond ((and (number? m1) (number? m2)) (* m1 m2))
          ((number? m1) (cond ((= m1 0) 0)
                              ((= m1 1) m2)
                              ((product? m2)
                               (cond ((number? (firstop m2))
                                      (make-product
                                       (* m1 (firstop m2)) (secondop m2)))
                                     ((number? (secondop m2))
                                      (make-product
                                       (* m1 (secondop m2)) (firstop m2)))
                                     (else worstcase)))
                              ((quotient? m2)
                               (cond ((number? (firstop m2))
                                      (make-quotient
                                       (* m1 (firstop m2)) (secondop m2)))
                                     ((number? (secondop m2))
                                      (make-product
                                       (/ m1 (secondop m2)) (firstop m2)))
                                     (else worstcase)))
                              (else worstcase)))
          ((number? m2) (cond ((= m2 0) 0)
                              ((= m2 1) m1)
                              ((product? m1)
                               (cond ((number? (firstop m1))
                                      (make-product
                                       (* m2 (firstop m1)) (secondop m1)))
                                     ((number? (secondop m1))
                                      (make-product
                                       (* m2 (secondop m1)) (firstop m1)))
                                     (else worstcase)))
                              ((quotient? m1)
                               (cond ((number? (firstop m1))
                                      (make-quotient
                                       (* m2 (firstop m1)) (secondop m1)))
                                     ((number? (secondop m1))
                                      (make-product
                                       (/ m2 (secondop m1)) (firstop m1)))
                                     (else worstcase)))
                              (else worstcase)))
          ((and (quotient? m1) (equal? (firstop m1) 1))
           (make-quotient m2 (secondop m1)))
          ((and (quotient? m2) (equal? (firstop m2) 1))
           (make-quotient m1 (secondop m2)))
          (else worstcase))))

(define (make-quotient m1 m2)
  (let ((worstcase (make-worstcase m1 '/ m2)))
    (cond ((and (number? m1) (number? m2)) (/ m1 m2))
          ((equal? m1 m2) 1)
          ((number? m1) (cond ((= m1 0) 0)
                              ((product? m2)
                               (cond ((number? (firstop m2))
                                      (make-quotient
                                       (/ m1 (firstop m2)) (secondop m2)))
                                     ((number? (secondop m2))
                                      (make-quotient
                                       (/ m1 (secondop m2)) (firstop m2)))
                                     (else worstcase)))
                              ((quotient? m2)
                               (cond ((number? (firstop m2))
                                      (make-product
                                       (/ m1 (firstop m2)) (secondop m2)))
                                     ((number? (secondop m2))
                                      (make-quotient
                                       (* m1 (secondop m2)) (firstop m2)))
                                     (else worstcase)))
                              (else worstcase)))
          ((number? m2) (cond ((= m2 1) m1)
                              ((product? m1)
                               (cond ((number? (firstop m1))
                                      (make-product
                                       (/ (firstop m1) m2) (secondop m1)))
                                     ((number? (secondop m1))
                                      (make-product
                                       (/ (secondop m1) m2) (firstop m1)))
                                     (else worstcase)))
                              ((quotient? m1)
                               (cond ((number? (firstop m1))
                                      (make-quotient
                                       (/ (firstop m1) m2) (secondop m1)))
                                     ((number? (secondop m1))
                                      (make-quotient
                                       (firstop m1) (* m2 (secondop m1))))
                                     (else worstcase)))
                              (else worstcase)))
          (else worstcase))))

(define (make-exponentiation b e)
  (let ((worstcase (make-worstcase b '** e)))
    (cond ((and (number? b) (number? e) (expt b e)))
          ((and (exponentiation? b) (number? (secondop b)) (number? e))
           (list (firstop b) '** (* (secondop b) e)))
          ((and (function? b) (eq? (ffunc b) 'sqrt) (= e 2)) (fop b))
          ((number? b) (if (or (= b 0) (= b 1)) 1 worstcase))
          ((number? e) (cond ((= e 0) 1)
                             ((= e 1) b)
                             (else worstcase)))
          (else worstcase))))

(define (make-logarithm b arg)
  (let ((worstcase (make-worstcase b 'log arg)))
    (cond ((and (number? b) (number? arg)) (/ (log arg) (log b)))
          ((equal? arg 1) 0)
          ((equal? b arg) 1)
          (else worstcase))))

(define (make-worstcase one two three)
  (cond ((equal? order 'prefix) (list two one three))
        ((equal? order 'infix) (list one two three))
        ((equal? order 'postfix) (list one three two))
        (else (error "Unknown ordering."))))

(define (firstop x)
  (cond ((equal? order 'prefix) (cadr x))
        ((equal? order 'infix) (car x))
        ((equal? order 'postfix) (car x))
        (else (error "Unknown ordering."))))

(define (secondop x)
  (cond ((equal? order 'prefix) (caddr x))
        ((equal? order 'infix) (caddr x))
        ((equal? order 'postfix) (cadr x))
        (else (error "Unknown ordering."))))

(define (func x)
  (cond ((equal? order 'prefix) (car x))
        ((equal? order 'infix) (cadr x))
        ((equal? order 'postfix) (caddr x))
        (else (error "Unknown ordering."))))

(define (sum? x)
  (if (not (atom? x)) (eq? (func x) '+) #f))

(define (difference? x)
  (if (not (atom? x)) (eq? (func x) '-) #f))

(define (product? x)
  (if (not (atom? x)) (eq? (func x) '*) #f))

(define (quotient? x)
  (if (not (atom? x)) (eq? (func x) '/) #f))

(define (exponentiation? x)
  (if (not (atom? x)) (eq? (func x) '**) #f))

(define (logarithm? x)
  (if (not (atom? x)) (eq? (func x) 'log) #f))

(define (ffunc x) (car x))

(define (fop x) (cadr x))

(define arctan atan)

(define (function? x)
  (if (not (atom? x))
      (case (ffunc x)
        ((sin cos sqrt arctan) #t)
        (else #f))))

(define (make-function f arg)
  (if (constant? arg)
      ((eval f) arg)
      (list f arg)))

(define (derive-x n f)
  (if (= n 0)
      f
      (derive-x (- n 1) (deriv f 'x))))

(define (derivations-x function start end)
  (do ((n start (+ n 1)))
      ((> n end) 'done)
      (display n)
      (display " ")
      (display (derive-x n function))
      (newline)))



