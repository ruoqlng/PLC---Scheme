;-----------------------------------------------
; CONTINUATIONS REPRESENTED BY SCHEME PROCEDURES
;-----------------------------------------------
(define apply-k
  (lambda (k v)
    (k v)))

(define make-k    
  (lambda (v) v))

(define make-cps
  (lambda (f)
    (lambda (x k)
      (apply-k k (f x)))))

(define null?-cps (make-cps null?))
(define car-cps (make-cps car))
(define cdr-cps (make-cps cdr))
(define cons-cps
  (lambda (x y k)
    (apply-k k (cons x y))))
(define eq?-cps
  (lambda (x y k)
    (apply-k k (eq? x y))))


; rewrite this in "fully CPSed" form
(define memq-cps
  (lambda (sym ls k)
  	(null?-cps ls
  		(make-k (lambda (ifnull)
  			(if ifnull     
	   			(apply-k k #f)
	   	(car-cps ls
	   		(make-k (lambda (ifcar)
	  			(eq?-cps ifcar sym
	  				(make-k (lambda (ifeq)
	  					(if ifeq
	  						(apply-k k #t)
	  	(cdr-cps ls
	  		(make-k (lambda (ifcdr)
				(memq-cps sym ifcdr k)))))))))))))))))
		     
; For extra credit, rewrite this in "fully CPSed" form
(define intersection-cps
  (lambda (los1 los2 k)
  	(null?-cps los1)
  	 (make-k (lambda (ifnull)
  	 	(if ifnull)
			(apply-k k '()) 
		(cdr-cps los1)
			(make-k (lambda (ifcdr)
				(if ifcdr)
					(intersection-cps ifcdr los2 k)
		(cdr los1) los2
	    (make-k (lambda (cdr-intersection)
		      (memq-cps (car los1) los2
				(make-k (lambda (is-in?)
					  (apply-k k
						   (if is-in?
						       (cons (car los1)
							     cdr-intersection)
						       cdr-intersection)))))))))))

(define intersection-cps
  (lambda (los1 los2 k)
    (if (null? los1) 
	(apply-k k '())
	(intersection-cps (cdr los1) los2
	    (make-k (lambda (cdr-intersection)
		      (memq-cps (car los1) los2
				(make-k (lambda (is-in?)
					  (apply-k k
						   (if is-in?
						       (cons (car los1)
							     cdr-intersection)
						       cdr-intersection)))))))))))
	