; Sample Scheme code to test scheme.lsp or tail.lsp

(define add-c (lambda (c) (lambda (n) (+ c n))))

(let ((compose (lambda (f g) (lambda (x) (f (g x))))))
  ((compose (add-c 5) (add-c 3)) 2))

(define Y                       ; The famous Y combinator!
  (lambda (f)
    (let ((future
            (lambda (future)
              (f (lambda (arg) 
                   ((future future) arg))))))
      (future future))))

((Y (lambda (factorial)
      (lambda (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1)))))))
 3)
