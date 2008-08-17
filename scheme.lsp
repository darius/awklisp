; This ain't a Scheme interpreter, despite the filename.
; Uses: startup scmhelp.lsp

(define read-eval-print-loop
  (lambda ()
    (let ((exp '*))
      (while (begin 
               (write '>) 
               (not (eq? the-eof-object (set! exp (read)))))
        (print (eval-exp (macroexpand exp) init-env))))))

(define eval-exp
  (lambda (exp env)
    ((evaluator exp) exp env)))

(define evaluator
  (lambda (exp)
    (if (atom? exp)
        (if (symbol? exp) lookup-variable self-evaluating)
        (or (get (car exp) 'evaluator)
            (lambda (exp env)
              (apply-proc (eval-exp (car exp) env)
                          (map (lambda (rand) (eval-exp rand env)) (cdr exp))))))))

(define self-evaluating
  (lambda (exp env) exp))

(put 'quote 'evaluator
  (lambda (exp env) (cadr exp)))

(put 'lambda 'evaluator make-closure)

(put 'if 'evaluator 
  (lambda (exp env)
    (if (eval-exp (test-exp exp) env)
        (eval-exp (then-exp exp) env)
        (eval-exp (else-exp exp) env))))

(put 'define 'evaluator
  (lambda (exp env)
    (define-variable-value (cadr exp) (eval-exp (caddr exp) env) env)
    (cadr exp)))

(define apply-proc
  (lambda (proc args)
    (if (primitive? proc)
        (apply proc args)
        (eval-exp (closure-body proc)
                  (extend-env (closure-formals proc) args (closure-env proc))))))

; Here we go

(define init-env (extend-env '() '() '()))

(read-eval-print-loop)
