(define make-queue
  (lambda ()
    (let ([first '()]
          [last '()])
      (lambda (msg . params)
        (cond 
          [(eqv? msg 'empty?) (null? first)]
          [(eqv? msg 'enqueue!) (set! first (cons first params))]
          [(eqv? msg 'dequeue!)
           (let ((v first))
             (set-car! first (cdr first))
             (car (cdr v)))])))))