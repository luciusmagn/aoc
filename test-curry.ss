(import (chezscheme))

(define-syntax fn
  (syntax-rules ()
    [(_ (name args ...) body)
     (define name
       (letrec ([f (case-lambda
                     [(args ...) body]
                     [partial-args
                      (lambda more-args
                        (apply f (append partial-args more-args)))])])
         f))]))

(define-syntax trace-block
  (syntax-rules ()
    [(_ expr ...)
     (begin
       (let ([result (begin
                       (display ">> ")
                       (write 'expr)
                       (newline)
                       expr)])
         (display "<< ")
         (write result)
         (newline))
       ...)]))


(fn (add x y)
    (+ x y))

(fn (add3 x y z)
    (+ x y z))

(trace-block
 (add 1 2)
 (add 1)
 ((add 1) 2)
 (add3 1 2 3)
 (add3 1)
 ((add3 1) 2 3))
