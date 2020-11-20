(load "chez-init.ss") ; put this file in the same folder, or add a pathname
 
; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.
 
; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.
 
(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (datum lit?)]
  [lambda-exp
   (id (list-of symbol?))
   (body (list-of expression?))]
  [lambdai-exp
   (id symbol?)
   (body (list-of expression?))]
  [app-exp
   (rator expression?)
   (rand expression?)]
  [if-else-exp
   (bool expression?)
   (true expression?)
   (false expression?)]
  [if-exp
   (bool expression?)
   (true expression?)]
  [set!-exp
   (id symbol?)
   (var expression?)]
  [let-exp
   (var (list-of (list-of expression?)))
   (body (list-of expression?))]
  [letrec-exp
   (var (list-of (list-of expression?)))
   (body (list-of expression?))]
  [let*-exp
   (var (list-of (list-of expression?)))
	(body (list-of expression?))]   
  )
 
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
 
 
(define lit?  ;;define literate type
  (lambda (exp)
    (or (number? exp)
	(string? exp)
	(char? exp)
	(vector? exp)
	(eq? (car exp) 'quote)
	(symbol? exp)
	(eq? #t exp)
	(eq? #f exp))))

(define list-of-symbols? 
  (lambda (lst)
    (if (list? lst)
	(andmap symbol? lst)
	#f)))

 
(define parse-exp         
  (lambda (datum)
    (cond
     [(and (pair? datum) (not (list? datum))) 
      (eopl:error 'parse-exp "Datum not a proper list!")]
     [(symbol? datum) (var-exp datum)]
     [(lit? datum) (lit-exp datum)]
     [(string? datum) (lit-exp datum)]
     [(pair? datum)
      (cond
       [(eqv? (1st datum) 'lambda)
	(cond 
	 [(null? (cddr datum)) (eopl:error 'parse-exp "Error 0:incorrect length in Lambda ~a" datum)] 
	 [(symbol? (2nd datum)) (lambdai-exp (2nd datum) (map parse-exp (cddr datum)))]
	 [else
	  (cond
	   [(list-of-symbols? (2nd datum)) 
	    (lambda-exp 
	     (2nd datum)
	     (map parse-exp (cddr datum)))]
	   [(not (list (2nd datum)));;error case
	    (eopl:error 'parse-exp "Error 1: Seems not a list structure at parsing lambda : ~a" (cadr datum))]
	   [else (eopl:error 'parse-exp "Error2: Seems not a symbol in lambda,cannot parse: ~a" (cadr datum))]
	   )])]
 
      
       [(eqv? (car datum) 'set!) 
	(if (equal? 3 (length datum)) 
	    (set-exp (2nd datum) (parse-exp (3rd datum)))
	    (eopl:error 'parse-exp "set! expression ~s does not have the required pair of variable and expression" datum))]


       [(eqv? (1st datum) 'let)
	(cond
	 [(or (null? (cdr datum)) (null? (cddr datum))) (eopl:error 'parse-exp "The length of the let expression ~s is incorrect." datum)]
	 [(not (list? (2nd datum))) (eopl:error 'parse-exp "let expression not a list" datum)]
	 [(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "let expression  declarations not a list of symbols" datum)]
	 [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "let expression declarations not a proper list" datum)]
	 [(not (andmap (lambda (a) (equal? (length a) 2)) (2nd datum))) (eopl:error 'parse-exp "let expression declarations not a list of 2" datum)]
	 [else (let-exp
		(map list (map var-exp (map car (2nd datum))) (map parse-exp (map 2nd (2nd datum))))
		(map parse-exp (cddr datum)))])]


	[(eqv? (1st datum) 'let*)
	 (cond
	  [(or (null? (cdr datum)) (null? (cddr datum)))
	   (eopl:error 'parse-exp "The length of the let* expression ~s is incorrect." datum)]
	  [(not (list? (2nd datum))) (eopl:error 'parse-exp "let* expression not a list" datum)]
	  [(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "let* expression declarations not a list of symbols" datum)]
	  [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "let* expression declarations not a proper list" datum)]
	  [(not (andmap (lambda (a) (equal? (length a) 2)) (2nd datum))) (eopl:error 'parse-exp "let* expression declarations not a list of 2" datum)]
	  [else (let*-exp
		 (map list (map var-exp (map car (2nd datum))) (map parse-exp (map 2nd (2nd datum))))
		 (map parse-exp (cddr datum)))])]

	[(eqv? (1st datum) 'letrec)
	 (cond
	  [(< (length datum) 3)
	   (eopl:error 'parse-exp "incorrect length: ~s" datum)]
	  [(or (not (list? (2nd datum))) (not (andmap list? (2nd datum))))
	   (eopl:error 'parse-exp "not a proper list: ~s" (2nd datum))]
	  [(not (andmap (lambda (x) (= 2 (length x))) (2nd datum)))
	   (eopl:error 'parse-exp "not length 2: ~s" (2nd datum))]
	  [(not (andmap symbol? (map 1st (2nd datum))))
	   (eopl:error 'parse-exp "first members must be symbols: ~s" datum)]
	  [else
	   (letrec-exp (map list (map parse-exp (map car (2nd datum))) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]

	[(eqv? (1st datum) 'if)
	 (cond
	  [(= 3 (length datum))
	   (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
	  [(= 2 (length datum))
	   (eopl:error 'parse-exp "missing then or else clauses: ~s" datum)]
	  [(> (length datum) 4)
	   (eopl:error 'parse-exp "too many parts: ~s" datum)]
	  [else
	   (if-else-exp (parse-exp (2nd datum))
			(parse-exp (3rd datum))
			(parse-exp (cadddr datum)))])]

	 [else (app-exp (parse-exp (1st datum))
			(parse-exp (2nd datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)]


)))

; An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda (x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))

   
 
(define (unparse-exp datum)
  (cases expression datum
	 [var-exp (id) id]
	 [lit-exp (datum) datum]                                          
	 [lambda-exp (id body)
		     (cons 'lambda
			    (cons id
				 (map unparse-exp body)))]
	 
	 [lambdai-exp (id body)
		      (cons 'lambda
			    (cons id
				  (map unparse-exp body)))]
	 [if-else-exp (bool true false)
		      (list 'if
			    (unparse-exp bool)
			    (unparse-exp true)
			    (unparse-exp false))]
	 [if-exp (bool true)
		 (list 'if
		       (unparse-exp bool)
		       (unparse-exp true))]
	 [set!-exp (id var)
		   (cons 'set!
			 (cons id
			       (unparse-exp var)))]
	 [app-exp (rator rand)
		  (cons (unparse-exp rator)
			(map unparse-exp rand))]
	 [let-exp (var body) 
		  (cons
		   'let
		   (cons (map 
		       (lambda (x)
			 	(map unparse-exp x))
		       var)
		      (map unparse-exp body)))]
 
	 [let*-exp (var body ) 
		   (cons
		    'let*
		    (cons(map 
		       (lambda (x)
			 	(map unparse-exp x))
		       var)
		      (map unparse-exp body)))]
 
	 [letrec-exp (var body) 
		     (cons
		      'letrec
		     (cons (map 
		       (lambda (x)
			 	(map unparse-exp x))
		       var)
		      (map unparse-exp body)))]))