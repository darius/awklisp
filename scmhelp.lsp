; Stuff common to scheme.lsp and tail.lsp.

; Environment operations

(define extend-env
  (lambda (vars vals env)
    (cons (cons vars vals) env)))

(define lookup-variable
  (lambda (var env)
    (if (null? env)
        (eval var)              ; to give access to Lisp primitives and constants
        (lookup-in-frame var (caar env) (cdar env) (cdr env)))))

(define lookup-in-frame
  (lambda (var vars vals enclosing-env)
    (if (null? vars)
        (lookup-variable var enclosing-env)
        (if (eq? (car vars) var)
            (car vals)
            (lookup-in-frame var (cdr vars) (cdr vals) enclosing-env)))))

(define define-variable-value
  (lambda (var value env)
    (let ((frame (car env)))
      (set-car! frame (cons var (car frame)))
      (set-cdr! frame (cons value (cdr frame))))))

; Syntax

(define cadar (lambda (lst) (cadr (car lst))))
(define cdddr (lambda (lst) (cddr (cdr lst))))
(define caddar (lambda (lst) (cadr (cdr (car lst)))))

(define test-exp cadr)
(define then-exp caddr)
(define else-exp (lambda (exp) (if (cdddr exp) (cadddr exp) nil)))
 
(define make-closure cons)
(define closure-formals cadar)
(define closure-body caddar)
(define closure-env cdr)

(define primitive?
  (lambda (object)
    (and (pair? object)
         (eq? (car object) '%prim))))

