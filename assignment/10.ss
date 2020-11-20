 ; Ruoqing Ouyang -Assignment 10

; #1
(define free-vars
  (lambda (LCExp)
    (letrec ([helper (lambda (LCExp bound x)
		       (cond
			[(not (pair? LCExp)) 
			 (if (member LCExp bound)
			     '()
			     (list LCExp))]
			[(eq? 'lambda (car LCExp))
			 (append
			  (helper (caddr LCExp)
			   (append (cadr LCExp) bound) x) x)]
			[else 
			 (append (helper (car LCExp) bound x)
				 (append
				  (helper (cadr LCExp) bound x) x))]))]
	     [noDup (lambda (x ls)
			   (if (null? x)
			       ls
			       (let ((item (car x)))
				 (if (member item ls)
				     (noDup (cdr x) ls)
				     (noDup (cdr x) (cons item ls))))))])
      (noDup (helper LCExp '() '()) '()))))

; #2
(define bound-vars
  (lambda (LCExp)
    (letrec ((helper (lambda (LCExp bound x)
		       (cond
			[(not (pair? LCExp))
			 (if (member LCExp bound)
			     (list LCExp)
			     '())]
			[(eq? 'lambda (car LCExp))
			 (append (helper (caddr LCExp) (append (cadr LCExp) bound) x) x)]
			[else
			 (append (helper (car LCExp) bound x)
				 (append (helper (cadr LCExp) bound x) x))]))))
      (helper LCExp '() '()))))

; #3
(define occurs-free?
  (lambda (var exp)
    (cond
     [(symbol? exp) (eqv? var exp)]
     [(eqv? (car exp) 'lambda)
      (and (not (member var (cadr exp))) (occurs-free? var (caddr exp)))]
     [(eqv? (car exp) 'let)
      (cond
       [(member var (map car (cadr exp)))
	(and (ormap (lambda(x)
		      (occurs-free? var x)) (map cadr (cadr exp)))
	     (occurs-free? var (caddr exp)))]
       [else
	(ormap (lambda(x) (occurs-free? var x)) (map cadr (cadr exp)))])]
     [(eqv? (car exp) 'let*)
      (cond
       [(member var (map cadr (cadr exp))) (not (member var (map car (cadr exp))))]
            [else (and (not (member var (map car (cadr exp))))
		       (occurs-free? var (caddr exp)))])]
     [(eqv? (car exp) 'set!) (and (not (eqv? var (cadr exp))) (eqv? var (caddr exp)))]
     [(eqv? (car exp) 'if) (ormap (lambda(x) (occurs-free? var x)) (cdr exp))]
     [else (ormap (lambda (x) (occurs-free? var x)) exp)])))
(define occurs-bound?
  (lambda (var exp)
    (cond
     [(symbol? exp) #f]
      [(eqv? (car exp) 'lambda) (or (occurs-bound? var (caddr exp))
				    (and (member var (cadr exp)) (occurs-free? var (caddr exp))))]
      [(eqv? (car exp) 'let)
       (if (member var (map car (cadr exp))) (occurs-free? var (caddr exp))
	   (occurs-bound? var (caddr exp)))]
      [(eqv? (car exp) 'let*)
       (if (and (member var (map car (cadr exp))) (member var (map cadr (cadr exp))))
	   #t
	   #f)]
      [(eqv? (car exp) 'set!)
       (and (equal? var (car exp)) (not (equal? var 'set!)))]
      [(eqv? (car exp) 'if)
       (occurs-bound? var (cdr exp))]
      [else (ormap (lambda(x) (occurs-bound? var x)) exp)])))

; #4
(define get-pos
  (lambda (ls item)
    (cond
     [(null? ls) -1]
     [(eq? (car ls) item) 0]
     [else
      (if (= (get-pos (cdr ls) item) -1)
	  -1
	  (add1 (get-pos (cdr ls) item)))])))
(define bound
  (lambda (item vars depth)
    (if (null? vars)
	(list ': 'free item)
	(let ([pos (get-pos (car vars) item)])
	  (if (= -1 pos)
	      (bound item (cdr vars) (add1 depth))
	      (list ': depth pos))))))
(define lexical-address
  (lambda (exp)
    (letrec ([create-addr
	      (lambda (exp vars)
		(cond
		 [(symbol? exp) (bound exp vars 0)]
		 [(eq? (car exp) 'lambda)
		  (list 'lambda (cadr exp) (create-addr (caddr exp) (cons (cadr exp) vars)))]
		 [(eq? (car exp) 'if)
		  (cons'if (create-addr (cdr exp) vars))]
		 [(eq? (car exp) 'let)
		  (let ([new-vars (map car (cadr exp))])
		    (list 'let
			  (map (lambda (x) (list (car x) (create-addr (cadr x) vars)))
			       (cadr exp))
			  (create-addr (caddr exp) (cons new-vars vars))))]
		 [(eq? (car exp) 'set!)
		  (list 'set! (cadr exp) (create-addr (caddr exp) vars))]
		 [else
		  (map (lambda (x) (create-addr x vars)) exp)]))])
      (create-addr exp '()))))

; #5
(define un-lexical-address
  (lambda (exp)
    (letrec ([helper 
	      (lambda (exp vars)
		(cond
		 [(null? (cdr exp))
		  (list (helper (car exp) vars))]
		 [(and (eq? (car exp) ':)
		       (eq? (cadr exp) 'free))
		  (caddr exp)]
		 [(and (eq? (car exp) ':)
		       (number? (cadr exp)))
		  (list-ref (list-ref vars (cadr exp)) (caddr exp))]
		 [(eq? (car exp) 'lambda)
		  (list 'lambda (cadr exp) (helper (caddr exp) (cons (cadr exp) vars)))]
		 [(eq? (car exp) 'if)
		  (cons 'if
			(cons (helper (cadr exp) vars)
			      (helper (cddr exp) vars)))]
		 [(eq? (car exp) 'let)
		  (list 'let 
			(map (lambda (x) (list (car x) (helper (cadr x) vars))) (cadr exp))
			(helper (caddr exp) (cons (map car (cadr exp)) vars)))]
		 [(eq? (car exp) 'set!)
		  (cons 'set! (cons (cadr exp) (helper (cddr exp) vars)))]
		 [else
		  (map (lambda (x) (helper x vars)) exp)]))])
      (helper exp '()))))
