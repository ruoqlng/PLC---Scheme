 ; Ruoqing Ouyang -Assignment 11a

; #1
;a
(define-syntax my-let
  (syntax-rules ()
    [(_ ([x e] ...) e1 e2 ...)
     ((lambda (x ...)
	e1 e2 ...)
      e ...)]
    [(_ n ([x e] ...) e1 e2 ...)
     (letrec ([n 
	       (lambda (x ...)
		 e1 e2 ...)])
       (n e ...))]))

;b
(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 ...)
     (let ([x e1])
       (if x
	   x
	   (my-or e2 ...)))]))

;c
(define-syntax +=
  (syntax-rules ()
    [(_ x y)
     (begin (set! x (+ x y))
	    x)]))

;d
(define-syntax return-first
  (syntax-rules ()
    [(_ e1 e2 ...)
     (let ([first e1])
       (begin e2 ...)
       first)]))

; #2

(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left-tree bintree?)
    (right-tree bintree?)))

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
	   [leaf-node (x)
		      (list 'leaf-node x)]
	   [interior-node (key left right)
			  (list 'interior-node key
				(bintree-to-list left)
				(bintree-to-list right))])))
; #3
(define max-interior
  (lambda (tree)
    (letrec ([helper 
               (lambda (tree)
                 (cases bintree tree
                   [leaf-node (datum) datum]
                   [interior-node (key left right)
                     (let ([left-ls (helper left)]
                           [right-ls (helper right)])
                       (cond
                       		[(and (number? left-ls) (number? right-ls))
                        		(list key (+ left-ls right-ls) key (+ left-ls right-ls))]
                       		[(and (number? left-ls) (list? right-ls))
                        		(let ([rkey (car right-ls)]
                               	[r-max (cadr right-ls)]
                               	[r-cur (cadddr right-ls)])
                           	(if (> r-max (+ left-ls r-cur))
                               	(list rkey r-max key (+ left-ls r-cur))
                            				(list key (+ left-ls r-cur) key (+ left-ls r-cur))))]
                         [(and (number? right-ls) (list? left-ls))
                         	(let ([lkey (car left-ls)]
                               	[l-max (cadr left-ls)]
                                [l-cur (cadddr left-ls)])
                           	(if (< l-max (+ right-ls l-cur))
                               	(list key (+ right-ls l-cur) key (+ right-ls l-cur))
                              		(list lkey l-max key (+ right-ls l-cur))))]
                       		[else
                         		(let ([lkey (car left-ls)]
                               		[rkey (car right-ls)]
                                	[cur-max (+ (cadddr left-ls) (cadddr right-ls))]
                                	[l-max (cadr left-ls)]
                                	[r-max (cadr right-ls)])
                            	(if (>= l-max r-max)
                               		(if (> l-max cur-max)
                                    	(list lkey l-max key cur-max)
                                   		(list key cur-max key cur-max))
                               		(if (< r-max cur-max)
                                   		(list key cur-max key cur-max)
                                    	(list rkey r-max key cur-max))))]))]))])
      (car (helper tree)))))

