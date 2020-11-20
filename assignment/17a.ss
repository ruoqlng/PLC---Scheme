;:  Single-file version of the interpreter.
;; Easier to submit to server, probably harder to use in the development process

;(load "chez-init.ss") 

;-------------------+
;                   |
;    DATATYPES      |
;                   |
;-------------------+

; parsed expression.  You'll probably want to replace this 
; code with your expression datatype from A11b

(define-datatype expression expression?
    [var-exp
        (id symbol?)]
    [lit-exp
        (datum lit?)]
    [lambda-exp
        (id (list-of symbol?))
        (body (list-of expression?))]
    [lambdai-exp
        (id pair?)
        (body (list-of expression?))]
    ; [lambdas-exp
    ;     (id symbol?)
    ;     (body (list-of expression?))]
    [app-exp
        (rator expression?)
        (rand (list-of expression?))]
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
        (var (list-of (list-of (lambda (x) (or (symbol? x) (expression? x))))))
        (body (list-of expression?))]
    [letrec-exp
        (var (list-of (list-of (lambda (x) (or (symbol? x) (expression? x))))))
        (body (list-of expression?))]
    [let*-exp
        (var (list-of (list-of (lambda (x) (or (symbol? x) (expression? x))))))
        (body (list-of expression?))]
    [quote-exp 
        (datum scheme-value?)]
    [begin-exp
        (body (list-of expression?))]
    [cond-exp
        (body (list-of (list-of expression?)))]
    [and-exp
        (body (list-of expression?))]
    [or-exp
        (body (list-of expression?))]
    [case-exp
        (test expression?)
        (body (list-of (lambda (x) (and (list? (car x)) ((list-of expression?) (cdr x))))))
        (els (list-of expression?))]
    [while-exp
        (test expression?)
        (body expression?)]
    [named-let-exp
        (var symbol?)
        (ids (list-of symbol?))
        (values (list-of expression?))
        (bodies (list-of expression?))]

    [define-exp
        (var symbol?)
        (body expression?)]
        
        
        
        )
	

    
;; environment type definitions

(define scheme-value?
    (lambda (x) #t))
  
(define-datatype environment environment?
    [empty-env-record]
    [extended-env-record
        (syms (list-of symbol?))
        (vals (list-of box?))
        (env environment?)])


; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
    [prim-proc
        (name symbol?)]
    [closure
        (id (list-of symbol?))
        (bodies (list-of expression?))
        (env environment?)]
    [improper-closure
        (id pair?)
        (bodies (list-of expression?))
        (env environment?)]
)

  
;-------------------+
;                   |
;    PARSER         |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

; Again, you'll probably want to use your code form A11b

(define lit?  ;;define literate type
    (lambda (exp)
        (or 
        (null? exp)
        (number? exp)
	    (string? exp)
	    (char? exp)
	    (vector? exp)
        (boolean? exp)
	    (eq? (car exp) 'quote)
	    (symbol? exp))
    )
)

(define list-of-symbols? 
    (lambda (lst)
        (if (list? lst)
	        (andmap symbol? lst)
	            #f)
    )
)

(define parse-cond
    (lambda (datum)
        (cond
            [(equal? (car datum) 'else) (cons (lit-exp #t) (map parse-exp (cdr datum)))]
            [else (map parse-exp datum)]
        )
    )
)

(define parse-exp         
 (lambda (datum)
  (cond
    [(and (pair? datum) (not (list? datum))) 
     (eopl:error 'parse-exp "Datum not a proper list!")]
    [(symbol? datum) (var-exp datum)]
    [(lit? datum) (lit-exp datum)]
    [(list? datum)
     (cond
        [(andmap (lambda (x)
            (ormap 
                (lambda (check) (check x))
                (list number? string? vector? boolean?))) datum) (lit-exp datum)] 
        [(eqv? (1st datum) 'lambda)
            (cond
                [(not (> (length datum) 2))
                    (eopl:error 'parse-exp "Error in parse-exp: incorrect length:" datum)] 
                [(null? (cddr datum)) (eopl:error 'parse-exp "Error 0:incorrect length in Lambda ~a" datum)] 
                [(symbol? (2nd datum)) (lambdai-exp (list (2nd datum)) (map parse-exp (cddr datum)))]
                [(and (not (list? (2nd datum))) (pair? (2nd datum)))
                    (lambdai-exp (2nd datum) (map parse-exp (cddr datum)))]
                [else
                    (cond
                    [(list-of-symbols? (2nd datum)) 
                        (lambda-exp 
                        (2nd datum)
                        (map parse-exp (cddr datum)))]
                    [(not (list (2nd datum)));;error case
                        (eopl:error 'parse-exp "Error 1: Seems not a list structure at parsing lambda : ~a" (cadr datum))]
                    [else (eopl:error 'parse-exp "Error2: Seems not a symbol in lambda,cannot parse: ~a" (cadr datum))])])]
        [(eqv? (car datum) 'set!) 
            (if (and (eq? (length datum) 3) (symbol? (2nd datum))) 
                (set!-exp (2nd datum) (parse-exp (3rd datum)))
                (eopl:error 'parse-exp "set! expression ~s does not have the required pair of variable and expression" datum))]
        [(eqv? (1st datum) 'let)
            (cond
                [(< (length datum) 3) (eopl:error 'parse-exp "incorrect length: ~s" datum)]
                [(or (null? (cdr datum)) (null? (cddr datum))) (eopl:error 'parse-exp "The length of the let expression ~s is incorrect." datum)]
                [(symbol? (2nd datum)) (named-let-exp (2nd datum) (map car (3rd datum)) (map parse-exp (map 2nd (3rd datum))) (map parse-exp (cdddr datum)))]
                [(not (list? (2nd datum))) (eopl:error 'parse-exp "let expression not a list" datum)]
                [(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "let expression  declarations not a list of symbols" datum)]
                [(not (andmap list? (2nd datum))) (eopl:error 'parse-exp "let expression declarations not a proper list" datum)]
                [(not (andmap (lambda (a) (equal? (length a) 2)) (2nd datum))) (eopl:error 'parse-exp "let expression declarations not a list of 2" datum)]
                [else (let-exp
                    (map list  (map car (2nd datum)) (map parse-exp (map 2nd (2nd datum))))
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
                    (map list  (map car (2nd datum)) (map parse-exp (map 2nd (2nd datum))))
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
                    (letrec-exp (map list  (map car (2nd datum)) (map parse-exp (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))])]
        [(eqv? (1st datum) 'quote) (quote-exp (2nd datum))]
        [(eqv? (1st datum) 'begin) (begin-exp (map parse-exp (cdr datum)))]
        [(eqv? (1st datum) 'cond) (cond-exp (map parse-cond (cdr datum)))]
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
        [(eqv? (1st datum) 'and) (and-exp (map parse-exp (cdr datum)))]
        [(eqv? (1st datum) 'or) (or-exp (map parse-exp (cdr datum)))]
        [(eqv? (1st datum) 'case)
            (case-exp (parse-exp (2nd datum)) (map parse-case (cddr datum)) (case-else (cddr datum)))]
        [(eqv? (1st datum) 'while)
            (while-exp (parse-exp (2nd datum)) (begin-exp (map parse-exp (cddr datum))))]
        [(eqv? (car datum) 'define)
	        (define-exp (cadr datum) (parse-exp (caddr datum)))]
        [else (app-exp (parse-exp (1st datum))
                (map parse-exp (cdr datum)))])]
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
(define parse-case
    (lambda (datum)
        (cond
            [(equal? (car datum) 'else) '(())]
            [else (cons (car datum) (map parse-exp (cdr datum)))])))
(define case-else
    (lambda (cc)
        (cond
            [(null? cc) (lit-exp (void))]
            [(equal? 'else (caar cc)) (map parse-exp (cdar cc))]
            [else (case-else (cdr cc))])))

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
            (cons   (map 
                        (lambda (x)
                            (map unparse-exp x))
                    var)
                    (map unparse-exp body)))]
    
        [let*-exp (var body ) 
            (cons
                'let*
                (cons (map 
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
	



;-------------------+
;                   |
;   ENVIRONMENTS    |
;                   |
;-------------------+





; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3
(define *prim-proc-names* '(append eqv? list-tail apply map + - * add1 sub1 cons
				   assq atom? = / not > < >= <= zero? not 
				   cons car cdr caar cadr caaar caadr cadar caddr 
				   cdaar cdadr cddar cdddr list null? eq? eqv? equal? length 
				   list->vector list? pair? vector->list vector vector-set! vector? vector-ref
				   number? symbol? list-tail procedure? member newline set-car! set-cdr!
				   vector vector-ref vector-set! quotient))

(define empty-env
    (lambda ()
        (empty-env-record)))

(define extend-env
    (lambda (syms vals env)
        (extended-env-record syms (map box vals) env)))

(define list-find-position
    (lambda (sym los)
        (let loop ([los los] [pos 0])
            (cond [(null? los) #f]
                [(eq? sym (car los)) pos]
                [else (loop (cdr los) (add1 pos))]))))
	    
(define apply-env-ref
    (lambda (env sym succeed fail) 
        (cases environment env 
            [empty-env-record () (fail)]
            [extended-env-record (syms vals env)
                (let ((pos (list-find-position sym syms)))
                    (if (number? pos)
                        (succeed (list-ref vals pos))
                        (apply-env-ref env sym succeed fail)))])))

(define apply-env 
    (lambda (env var succeed fail) 
        (deref (apply-env-ref env var succeed fail)))) 

(define deref
    (lambda (ref)
        (if (box? ref)
            (unbox ref)
            ref)))

(define set-ref!
    (lambda (box ref)
    (if 
    (equal? (void) box)
    ref
    (set-box! box ref)
)))


;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+

; To be added in assignment 14.

(define syntax-expand
    (lambda (exp)
        (cases expression exp
            [cond-exp (body) (cond-loop body)]
            [begin-exp (body) (app-exp (lambda-exp '() (map syntax-expand body)) '())]
            [and-exp (body)
                (cond
                    [(null? body) (lit-exp #t)]
                    [(null? (cdr body)) (syntax-expand (car body))]
                    [else (syntax-expand (if-else-exp (car body) (and-exp (cdr body)) (lit-exp #f)))])]
            [or-exp (body)
                (cond
                    [(null? body) (lit-exp #f)]
                    [(null? (cdr body)) (syntax-expand (car body))]
                    [else 
                    (syntax-expand (let-exp (list (list 'boole (car body))) (list (if-else-exp (var-exp 'boole) (var-exp 'boole) (syntax-expand (or-exp (cdr body)))))))])]
            ; [or-exp (body)
            ;     (if (null? body)
            ;     #f
            ;     (let ([next (eval-exp (syntax-expand (car body)) env)])
            ;         (if next next (eval-exp [or-exp (cdr body)] env))))] 
 
            [lambda-exp (id body) (lambda-exp id (map syntax-expand body))]
            [lambdai-exp (id body) (lambdai-exp id (map syntax-expand body))]
            [let-exp (var body)
                (app-exp
                    (lambda-exp (map 1st var) (map syntax-expand body)) (map syntax-expand (map 2nd var)))]
            [case-exp (test body els)
                (if (null? body)
                    (syntax-expand (begin-exp els))
                    (syntax-expand (if-else-exp (app-exp (var-exp 'member) (list test (quote-exp (caar body))))
                                        (begin-exp (cdar body))
                                        (syntax-expand (case-exp test (cdr body) els)))))]
            [set!-exp (id var) (set!-exp id (syntax-expand var))]
            [letrec-exp (var body) (syntax-expand (let-exp (map (lambda (x) (list (car x) (lit-exp #f))) var) (append (map (lambda (x) (set!-exp (car x) (2nd x))) var) body)))]
            [let*-exp (var body)
                (syntax-expand
                    (let-exp (list (car var))
                        (if (null? (cdr var))
                        body
                        (list (let*-exp (cdr var) body)))
                    )
                )
            ]
            [named-let-exp (var ids values bodies)
                (syntax-expand
                    (letrec-exp
                        (list (list var (lambda-exp ids bodies)))
                        (list (app-exp (var-exp var) values))))]
            [define-exp (var body)
	            (define-exp var (syntax-expand body))]
                        
            [else exp]
        )
    )
)



(define cond-loop
  (lambda (body)
    (if (null? (cdr body))
        (if-exp (caar body) (cadar body))
        (if-else-exp (caar body) (cadar body) (cond-loop (cdr body))))))
        
(define case-loop
  (lambda (arg body)
    (if (null? (cdr body))
        (1st (caddar body))
        (if-else-exp (app-exp (var-exp 'member) (list (var-exp 'x) (cadar body)))
        (1st (caddar body)) (case-loop arg (cdr body))))))

;--------------------------------------+
;                                      |
;   CONTINUATION DATATYPE and APPLY-K  |
;                                      |
;--------------------------------------+

; To be added in assignment 18a.


;-------------------+
;                   |
;   INTERPRETER     |
;                   |
;-------------------+


; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter
(define eval-bodies
  (lambda (body env)
    (cond
      [(null? (cdr body)) (eval-exp (1st body) env)]
      [else (begin (eval-exp (1st body) env) (eval-bodies (cdr body) env))])))

(define eval-exp
  (lambda (exp env)
    (cases expression exp
        [lit-exp (datum) 
            (if (pair? datum)
			    (cadr datum)
			datum)]
        [quote-exp (datum) datum]
        [var-exp (id) 
            (apply-env 
            env 
            id
            (lambda (x) x) ;if it's in the env
            (lambda ()
                (apply-env 
                global-env
                id
                (lambda (x) x)
                (lambda () ; call if id not in global-env
                        (eopl:error 'apply-env "variable ~s is not bound"
						       id)))))]
        [app-exp (rator rands)
            (let ([proc-value (eval-exp rator env)]
                [args (eval-rands rands env)])
            (apply-proc proc-value args))]
        ; [let-exp (var body) 
        ;     (eval-exp (syntax-expand exp) env)]
        [if-else-exp (bool true false)
            (if (eval-exp (syntax-expand bool) env)
                (eval-exp (syntax-expand true) env)
                (eval-exp (syntax-expand false) env))]
        [lambda-exp (id body)
            (closure id (map syntax-expand body) env)]
        [lambdai-exp (id body)
            (improper-closure id (map syntax-expand body) env)]
        [if-exp (bool true)
            (if (eval-exp (syntax-expand bool) env) 
            (eval-exp (syntax-expand true) env))]
        [begin-exp (body)
            (for-each (lambda (x) (eval-exp x env)) (map syntax-expand body))]
        [while-exp (test body)
            (letrec ([loop (lambda ()
                        (if (eval-exp test env)
                            (begin
                                (eval-exp (map syntax-expand body) env)
                                (loop))
                                ))]) 
                (loop))]


          [set!-exp (id var)
            (if (not (expression? (eval-exp (var-exp id) env)))
            (set-ref! (apply-env-ref env id 
                      (lambda (x) x)
                      (lambda ()
                      (set-ref! (apply-env-ref global-env id (lambda (x) x)
                      (lambda () (eopl:error 'apply-env "variable not found" id))) (eval-exp  (syntax-expand var) env))))
                    (eval-exp  (syntax-expand var) env)))
          ]
        [define-exp (var body)
	       (eval-define var (eval-exp body env))]
        [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (id bodies env)
        (eval-bodies bodies (extend-env id args env))]
      [improper-closure (id bodies env)
        (eval-bodies bodies (extend-env (proper-ids id) (proper-args args (length (proper-ids id))) env))]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))
; Makes an improper list of ids proper
(define proper-ids
    (lambda (ids)
        (cond
            [(null? ids) '()]
            [(pair? ids) (cons (car ids) (proper-ids (cdr ids)))]
            [else (list ids)])))
; Makes an improper list of args proper
(define proper-args
    (lambda (args count)
        (cond
            [(equal? 1 count) (list args)]
            [else (cons (car args) (proper-args (cdr args) (sub1 count)))])))


(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

(define global-env init-env)

(define (make-init-env)
  init-env)


(define (reset-global-env)
    (set! global-env (make-init-env)))

(define eval-define 
  (lambda (var body)
    (set! global-env
      (extend-env 
       (list var)
       (list body)
       global-env))))

     

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.



(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(zero?) (eq? (car args) 0)]
      [(>) (apply > args)]
      [(<) (apply < args)]
      [(>=) (apply >= args)]
      [(<=) (apply <= args)]
      [(not) (not (car args))]
      [(cons)(cons  (1st args) (2nd args))]
      [(car) (car(car args))]  
      [(cdr) (cdr (car args))]
      [(caar) (caar (car args))]
      [(cadr) (cadr (car args))]
      [(caaar) (caaar (car args))]
      [(cadar) (cadar (car args))]
      [(caadr) (caadr (car args))]
      [(caddr) (caddr (car args))]
      [(cdaar) (cdaar (car args))]
      [(cdadr) (cdadr (car args))]
      [(cddar) (cddar (car args))]
      [(cdddr) (cdddr (car args))]
      [(map) (apply map (lambda x (apply-proc (car args) x)) (cdr args))]
      [(apply) (apply-proc (car args) (cadr args))]
      [(list) args]
      [(null?) (null? (car args))] 
      [(eq?) 
       (display (car args)) (newline)
       (display (2nd args)) (newline)
       (eqv? (car args) (2nd args))]
      [(equal?)(if (null? (cdr args)) 
                            (error 'apply-prim-proc "equal? requires 2 args")
			    (equal? (car args) (2nd args)))]
      [(eqv?) 
        (if (null? (cdr args)) 
          (error 'apply-prim-proc "eqv? requires 2 args")
          (eqv? (1st args) (2nd args)))]
      [(atom?) (not (pair? args))]
      [(append) (apply append args)]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (car args))]
      [(list?) (list? (car args))]
      [(pair?) (pair? (car args))]
      [(vector->list) (vector->list (car args))]
      [(vector) (apply vector args)]
      [(vector?) (vector? (car args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(number?) (number? (car args))]
      [(symbol?) (symbol? (car args))]
      [(procedure?) (proc-val? (car args))]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(newline) (apply newline args)]
      [(member) (member (1st args) (2nd args))] 
      [(quotient) (apply quotient args)]
      [(list-tail) (apply list-tail args)]
      [(pair?) (pair? args)]
      [(assq) (apply assq args)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))


(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
    (lambda (exp)
        (top-level-eval (syntax-expand (parse-exp exp)))))