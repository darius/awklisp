; Tail-recursive Scheme interpreter
; Uses: startup scmhelp.lsp
; Based on Abelson & Sussman, chapter 5.
; The most glaring omissions are error-checking, set!, begin, and call/cc.

(define next '*)

(define exp '*)
(define env '*)
(define proc '*)
(define rands '*)
(define args '*)
(define value '*)
(define cont '*)

(define read-eval-print-loop
  (lambda ()
    (while (begin
             (write '>)
             (not (eq? the-eof-object (set! exp (read)))))
      (set! exp (macroexpand exp))
      (set! env init-env)
      (set! cont (lambda () 'halt))
      (set! next eval-exp)
      (run)
      (print value))))

(define run
  (lambda ()
    (while (not (eq? (next) 'halt)))))

(define goto
  (lambda (procedure)
    (set! next procedure)))

(define return
  (lambda (val)
    (set! value val)
    (set! next cont)))

(define eval-exp
  (lambda ()
    (if (atom? exp)
        (return
          (if (symbol? exp) 
              (lookup-variable exp env)
              exp))
        (let ((handler (get (car exp) 'evaluator)))
          (if handler
              (handler)
              (begin                    ; procedure call
                (push cont)             ; this eventually is popped by apply-proc
                (push env)
                (push (cdr exp))        ; save the operands
                (set! exp (car exp))    ; evaluate the operator
                (set! cont eval-rands)
                (goto eval-exp)))))))

(define eval-rands
  (lambda ()
    (set! rands (pop))
    (set! env (pop))
    (set! args '())
    (push value)                ; save the procedure
    (goto rands-loop)))

(define rands-loop
  (lambda ()
    (if (null? rands)
        (begin
          (set! args (reverse! args))
          (set! proc (pop))
          (goto apply-proc))
        (begin
          (set! exp (car rands))
          (push env)
          (push args)
          (push (cdr rands))
          (set! cont add-arg)
          (goto eval-exp)))))

(define add-arg
  (lambda ()
    (set! rands (pop))
    (set! args (cons value (pop)))
    (set! env (pop))
    (goto rands-loop)))

(put 'quote 'evaluator
  (lambda () 
    (return (cadr exp))))

(put 'lambda 'evaluator 
  (lambda ()
    (return (make-closure exp env))))

(put 'if 'evaluator 
  (lambda ()
    (push cont)
    (push env)
    (push exp)
    (set! cont decide)
    (set! exp (test-exp exp))
    (goto eval-exp)))

(define decide
  (lambda ()
    (set! exp (pop))
    (set! env (pop))
    (set! cont (pop))
    (set! exp (if value (then-exp exp) (else-exp exp)))
    (goto eval-exp)))

(put 'define 'evaluator
  (lambda ()
    (push cont)
    (push env)
    (push (cadr exp))                   ; save the variable being defined
    (set! exp (caddr exp))              ; evaluate the defining expression
    (set! cont do-definition)
    (goto eval-exp)))

(define do-definition
  (lambda ()
    (set! exp (pop))
    (set! env (pop))
    (set! cont (pop))
    (define-variable-value exp value env)
    (return exp)))

(define apply-proc
  (lambda ()
    (set! cont (pop))
    (if (primitive? proc)
        (return (apply proc args))
        (begin
          (set! exp (closure-body proc))
          (set! env 
            (extend-env (closure-formals proc) args (closure-env proc)))
          (goto eval-exp)))))

; Stack operations

(define stack '())

(define push
  (lambda (x)
    (set! stack (cons x stack))))

(define pop
  (lambda ()
    (let ((result (car stack)))
      (set! stack (cdr stack))
      result)))

; Here we go

(define init-env (extend-env '() '() '()))

(read-eval-print-loop)
