; a2 ouyangr
; #1
(define fact
	(lambda (num)
		(cond
			[(zero? num) 1]
			[else (* num (fact (- num 1)))]
		)
	)
)
(define choose
	(lambda (n k)
    	(/ (fact n) (* (fact k) (fact (- n k))))
    )
)
; #2
(define square
	(lambda (num)
		(* num num)
	)
)
(define sum-of-squares
    (lambda (lon)
    	(cond
    		[(null? lon) 0]
       		[else (+ (square (car lon)) (sum-of-squares (cdr lon)))]
       	)
    )
)
; #3
(define range
	(lambda (m n)
    	(cond
    		[(<= n m) '()]
    		[else (cons m (range (+ m 1) n))]
    	)
    )
)
; #4
(define set?
	(lambda (ls)
		(cond
  			[(null? ls) #t]
  			[(member (car ls) (cdr ls)) #f]
      		[else (set? (cdr ls))]
      	)
    )
)
; #5
(define union
	(lambda (l1 l2)    
    	(cond 
    		[(null? l1) l2]
      		[(member (car l1) l2) (union (cdr l1) l2)]
      		[else (union (cdr l1) (cons (car l1) l2))]
  		)
	)
)
; #6
(define cross-product
	(lambda (v1 v2)
      	(list 	(- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
      			(- (* (caddr v1) (car v2)) (* (car v1) (caddr v2))) 
      			(- (* (car v1) (cadr v2)) (* (cadr v1) (car v2)))
      	)
    )
)
; #7
(define parallel?
	(lambda (v1 v2)
    	(andmap = '(0 0 0) (cross-product v1 v2))
    )
)
; #8
(define make-vec-from-points
  	(lambda (l1 l2)
    	(list (- (car l2) (car l1)) (- (cadr l2) (cadr l1)) (- (caddr l2) (caddr l1)))
    )
)
(define collinear? 
	(lambda (p1 p2 p3)
    	(parallel? (make-vec-from-points p1 p2) (make-vec-from-points p1 p3))
    )
)