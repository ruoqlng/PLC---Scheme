; - Assignment 6b ouyangr
; #8
(define let->application
  (lambda (l)
    (append (list (list 'lambda (map car (cadr l))
			(cadr (cdr l)))) (map cadr(cadr l)))))
; #9
(define let*->let
  (lambda (l)
    (letrec ([one-let
	      (lambda (let e)
		(if (null? let)
		    e
		    (list 'let (list (car let)) (one-let (cdr let) e))))])
      (one-let (cadr l) (caddr l)))))
; #10
(define qsort
  (lambda (pred ls)
    (letrec ([sort-low
	      (lambda (pivot ls pred)
		(if (null? ls)
		    ls
		    (if (pred (car ls) pivot)
			(cons (car ls) (sort-low pivot (cdr ls) pred))
			(sort-low pivot (cdr ls) pred))))]
	     [sort-high
	      (lambda (pivot ls pred)
		(if (null? ls)
		    ls
		    (if (pred (car ls) pivot)
			(sort-high pivot (cdr ls) pred)
			(cons (car ls) (sort-high pivot (cdr ls) pred)))))])   
      (if (< (length ls) 2)
	  ls
	  (append (qsort pred (sort-low (car ls) (cdr ls) pred))
		  (cons (car ls)
			(qsort pred (sort-high (car ls) (cdr ls) pred))))))))
; #11 
(define sort-list-of-symbols
  (lambda (l)
    (map string->symbol (sort string<? (map symbol->string l)))))
