;1
  (define (map-in-order proc ls)
    (if (null? ls)
        '()
        (let ([first (proc (car ls))]) 
        	(cons first
              (map-in-order proc (cdr ls))))))

;2 
(define-syntax my-do
 (syntax-rules (while)
    ((_ (exp ...) while test)
     (begin exp ...
     	(let loop () (if test 
     					(begin exp ... (loop))))))))

;3

(define (make-queue)
	(let ([first '()] 
		  [last '()])
		(lambda (msg . param)
			(cond
				[(eqv? msg 'empty?) (null? first)]
				[(eqv? msg 'enqueue!) 
					(if (null? first)
						(begin (set! first param) (set! last first))
						(begin (set-cdr! last param) (set! last (cdr last))))]
				[(eqv? msg 'dequeue!) 
					(let ([helper (car first)])
						(set! first (cdr first))
						(if (null? first) (set! last '())) helper)]))))