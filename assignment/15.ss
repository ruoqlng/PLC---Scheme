; assignment 16 ouyangr

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define make-k (lambda (k) k))
(define apply-k (lambda (k v) (k v)))
(define apply-continuation (lambda (k v) (k v)))

(define exp?     
  	(lambda (obj)  
    	(or (symbol? obj)
    		(and (list? obj)
	     		(or 
	      			(and (= (length obj) 3)
		   				(eq? (1st obj) 'lambda)
		   				(list? (2nd obj))
		   				(= (length (2nd obj)) 1)
					   	(symbol? (caadr obj))
					   	(exp? (3rd obj)))
				      	(and (= (length obj) 2)
					   	(exp? (1st obj))
					   	(exp? (2nd obj))))))))

(define-datatype continuation continuation?
  	[init-k]
  	[list-k]
  	[not-k]
  	[union-k (s1 (list-of symbol?))
  		   (s2 (list-of symbol?))
  		   (k continuation?)]
  	[cdr-union-k (car-L symbol?)
  		   (is-in? boolean?)
  		   (k continuation?)]
  	[cons-k (result symbol?)
            (k continuation?)]
  	[remove-k (sym symbol?)
  			(k continuation?)]
  	[u-k (k continuation?)]
  	[u-2k (result (list-of symbol?))
  		(k continuation?)] 
  	[free-vars-k (sym exp?)
  			   (k continuation?)] 
  	)

(define apply-k-ds
  	(lambda (k v)
    	(cases continuation k
	     	[init-k () v]
	     	[list-k () (list v)]
	     	[not-k () (not v)]
		 	[union-k (s1 s2 k)
		 	 	(union-cps (cdr s1) s2 (cdr-union-k (car s1) v k))]
		 	[cdr-union-k (car-L is-in? k)
		 	 	(apply-k-ds k (if is-in? v (cons car-L v)))]
		 	[cons-k (result k)
		     	(apply-k-ds k (cons result v))]
		 	[remove-k (sym k)
		 	  	(remove-cps sym v k)]
		 	[free-vars-k (sym k)
		 		(free-vars-cps sym (u-2k v k))]
		 	[u-k (k)
		 	 	(u-2k v k)]
		 	[u-2k (result k)
		     	(union-cps v result k)]
)))

;1
;a
(define member?-cps
	(lambda (item ls k)
		(cond
			[(null? ls) (apply-continuation k #f)]
			[(eq? (car ls) item) (apply-continuation k #t)]
			[else (member?-cps item (cdr ls) k)])))

(define set?-cps
	(lambda (ls k)
		(cond
			[(null? ls) (apply-continuation k #t)]
			[(not (pair? ls)) (apply-continuation k #f)]
			[else (set?-cps (cdr ls) 
				(lambda (cdr-ls)
					(member?-cps (car ls) (cdr ls) 
						(lambda (appeared?)
							(if appeared? 
								(apply-continuation k #f)
								(apply-continuation k cdr-ls))))))])))

(define set-of-cps
  (lambda (s k)
    (if (null? s)
        (apply-continuation k '())
        (set-of-cps (cdr s) 
        	(lambda (cdr-s)
				(member?-cps (car s) (cdr s) 
					(lambda (x)
                    	(if x
                            (apply-continuation k cdr-s)
                            (apply-continuation k (cons (car s) cdr-s))))))))))

(define map-cps
	(lambda (proc-cps l k)
		(cond
			[(null? l) (apply-continuation k '())]
			[else (map-cps proc-cps (cdr l) 
				(lambda (x)
					(proc-cps (car l) 
						(lambda (res) 
							(apply-continuation k (cons res x))))))]
		)))

(define 1st-cps
	(lambda (ls k)
		(apply-continuation k (car ls))))

(define domain-cps
	(lambda (rel k)
		(map-cps 1st-cps rel 
			(lambda (x)
				(set-of-cps x k)))))

;b

(define make-cps
	(lambda (proc)
		(lambda (arg k)
			(apply-continuation k (proc arg)))))

;c

(define andmap-cps
	(lambda (pred-cps ls k)
		(cond
			[(null? ls)(apply-continuation k #t)]
			[(pred-cps (car ls) 
				(lambda (still-true)
					(if still-true
						(andmap-cps pred-cps (cdr ls) k)
						(apply-continuation k #f))))])))


;2

(define memq-cps
	(lambda (sym ls k)
		(cond 
			[(null? ls) (apply-k-ds k #f)]
 			[(eq? (car ls) sym) (apply-k-ds k #t)]
 			[else (memq-cps sym (cdr ls) k)])))

(define union-cps
	(lambda (s1 s2 k)
 			(cond 
 				[(null? s1) (apply-k-ds k s2)]
 				[else (memq-cps (car s1) s2 (union-k s1 s2 k))])))

(define remove-cps
	(lambda (sym los k)
 		(cond 
 			[(null? los) (apply-k-ds k '())]
 			[(eq? sym (car los)) (apply-k-ds k (cdr los))]
 			[else (remove-cps sym (cdr los) (cons-k (car los) k))])))

(define free-vars-cps 
 	(lambda (exp k) 
 		(cond 	
 			[(symbol? exp) (apply-k-ds k (list exp))]
 			[(eq? (1st exp) 'lambda) (free-vars-cps (3rd exp)
				(remove-k (car (2nd exp)) k))]
 			[else (free-vars-cps (2nd exp) 
 				(free-vars-k (1st exp) k))])))