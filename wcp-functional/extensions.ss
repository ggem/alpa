;;;
;;;
;;;

; properties
(load "proplist.ss")
(define getprop get)
(define putprop put)

; syntax-case
(define sc-expand #f)
(define identifier? #f)
(define datum->syntax-object #f)
(define syntax-object->datum #f)
(define generate-temporaries #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define syntax-error #f)
(define install-global-transformer #f)
(define syntax-dispatch #f)
(load "psyntax.ss")

(define-macro define-syntax
  (lambda (name expander)
    `(begin
       (sc-expand '(define-syntax ,name ,expander))
       (define-macro ,name
         (lambda args
           (sc-expand (cons ',name args)))))))

; eval
(current-eval
  (let ([old-eval (current-eval)])
    (case-lambda
      [(arg0) (old-eval (sc-expand arg0))]
      [(arg0 arg1) (old-eval (sc-expand arg0) arg1)])))

(load "synforms.ss")
(void)
