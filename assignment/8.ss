; -Assignment 8

; 1
(define make-stack
  (lambda ()
    (let ([s '()])
      (lambda (msg . arg )
	(case msg 
	  [(empty?) (null? s)]
	  [(push) (set! s (cons (car arg) s))]
	  [(pop) (let ([top (car s)])
		   (set! s (cdr s))
		   top)])))))
(define next
  (lambda (s)
    (if (s 'empty?)
	#f
	(let ([cur (s 'pop)])
	  (cond
	   [(null? cur) (next s)]
	   [(symbol? cur) cur]
	   [else (begin (s 'push (cdr cur))
			(s 'push (car cur))
			(next s))])))))
(define make-slist-leaf-iterator
  (lambda (slist)
    (let ([s (make-stack)])
      (begin (s 'push slist)
	     (lambda (msg)
	       (case msg
		 [(next) (next s)]))))))
; 2
(define subst-leftmost
  (lambda (new old slist equality-pred?)
    (letrec ([helper
	      (lambda (new old slist equality-pred?)
		(cond
		 [(null? slist) (cons #f slist)]
		 [(symbol? (car slist))
		  (if (equality-pred? old (car slist))
		      (cons #t (cons new (cdr slist)))
		      (let ([list-cdr
			     (helper new old (cdr slist) equality-pred?)])
			(cons (car list-cdr) (cons (car slist) (cdr list-cdr)))))]
		 [else
		  (let ([list-car
			 (helper new old (car slist) equality-pred?)])
		    (if (car list-car)
			(cons #t (cons (cdr list-car) (cdr slist)))
			(let ([list-cdr (helper new old (cdr slist) equality-pred?)])
			  (cons (car list-cdr) (cons (cdr list-car) (cdr list-cdr))))))]))])
      (cdr (helper new old slist equality-pred?)))))