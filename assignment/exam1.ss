; ouyangr
; C1 
(define (alternating-reverse lol)
	(cond
		[(null? lol) '()]
		[(null? (cdr lol)) (list (reverse-it (car lol)))]
		[(null? (cddr lol)) (list (reverse-it (car lol)) (cadr lol))]
		[else (append (list (reverse-it (car lol)) (cadr lol)) (alternating-reverse (cddr lol)))]
	)	
)

(define reverse-it
  (letrec ([reverse-helper
	    (lambda (l1 l2)
	      (if (null? l1) l2
		  (reverse-helper (cdr l1) (cons (car l1) l2))))])
    (lambda (l3)
      (reverse-helper l3 '()))))

;C2
(define (member-n? sym n los)
	(cond
		[(null? los) #f]
		[(zero? n) #t]
		[(and (equal? sym (car los)) (null? (cdr los))) #t]
		[(equal? sym (car los)) 
			(member-n? sym (- n 1) (cdr los))]
		[else (member-n? sym n (cdr los))]
	)
)

;c3
(define (opposites-attract ls)
	(let helper ([x ls] 
				 [l (- (length ls) 1)]  
				 [els '()] 
				 [i 0]
				)
		(if (null? x) els
		    (helper (cdr x) 
		    		l 
		    		(append els (cons (list (list-ref ls i) (list-ref ls (- l i))) '()))
		    		(+ i 1)
		    )
		)
	)
)

;c4
(define (symmetric? rel)
	(let helper ([cur rel])
		(cond
			[(null? cur) #t]
			[(member (list (cadr (car cur)) (car(car cur))) rel) (helper (cdr cur))]
			[else #f]
		)
	)
)

;c5
(define (lower-triangular? m)
		(helper m (- (length (car m)) 1) (- (length (car m)) 1)))
	
(define (helper m row col)
	(cond
		[(<= col 0) #t]
		[(< row 0) (helper m (- col 1) (- col 1))]
		[(= row col) (helper m (- row 1) col)]
		[(zero? (matrix-ref m row col)) (helper m (- row 1) col)]
		[else #f]
	)
)

(define (matrix-ref m row col)
  	(list-ref (list-ref m row) col))


;c6
;--------- BST procedures fromm HW7, some slightly modified  -------------

(define BST-node list)

(define (BST-leafnode n) ; make a new leaf node
  (BST-node n (empty-BST) (empty-BST)))


; Accessors for part of a node
(define BST-element car)
(define BST-left cadr)
(define BST-right caddr)

(define (BST-makenode num left right)
  	(list num left right))

; empty tree functions
(define (empty-BST) '())   ; make one
(define empty-BST? null?)  ; test one

(define (BST? obj)  ; Is this object a BST?
  (or (empty-BST? obj)
      (and (tree? obj) 	
	   (let ([inorder-list (BST-inorder obj)])
	     (and (set? inorder-list) 
		  (sorted? inorder-list))))))

(define (BST-insert num bst)
  (cond [(empty-BST? bst) (BST-leafnode num)]
	[(not (integer? num)) 
	 (error 'BST-insert 
		"attempt to insert non-integer into BST")]
	[(= num (BST-element bst)) bst]
	[(< num (BST-element bst))
	 (BST-node (BST-element bst) 
		   (BST-insert num 
			       (BST-left bst)) 
		   (BST-right bst))]
	[else 	 
	 (BST-node (BST-element bst) 
		   (BST-left bst) 
		   (BST-insert num 
			       (BST-right bst)))]))

(define (BST-leaf? bst)
  (and (list? bst)
       (= (length bst) 1)))

(define (BST-remove num bst)
	(cond
		[(empty-BST? bst) '()]
		[(not (integer? num)) (error 'BST-remove "attempt to remove non-integer into BST")]
		[(not (BST-contains? bst num)) (BST-leafnode num)]
		[(BST-leaf? bst) (BST-remove-leaf num bst)]
		[else (BST-remove-node num bst)]
	)
)

(define (BST-remove-leaf num bst)
  	(if (= num (car bst)) '()
      	bst))

(define (BST-remove-node num bst)
  	(let ((cur (car bst))
        (left (BST-left bst))
        (right (BST-right bst)))
    	(if (<= num cur)
        	(if (BST-leaf? left)
            	(if (= num (car left))
                	right
                	bst)
            	(BST-makenode cur (BST-remove-node num left) right))
        (if (BST-leaf? right)
            (if (= num (car right))
                left
                bst)
            (BST-makenode cur left (BST-remove-node num right))))))

(define (BST-preorder bst)
  (if (empty-BST? bst)
      '()
      (append (list (BST-element bst)) 
	      (BST-preorder (BST-left bst))
	      (BST-preorder (BST-right bst)))))

(define (BST-inorder bst)
  (if (empty-BST? bst)
      '()
      (append (BST-inorder (BST-left bst)) 
	      (list (BST-element bst))
	      (BST-inorder (BST-right bst)))))

(define (BST-contains? bst num)
  (cond [(empty-BST? bst) #f]
	[(= (BST-element bst) num) #t]
	[(< (BST-element bst) num) (BST-contains? (BST-right bst) num)]
	[else (BST-contains? (BST-left bst) num)]))

(define (BST-height bst)
  (if (empty-BST? bst)
      -1
      (+ 1 (max (BST-height (BST-left bst))
		(BST-height (BST-right bst))))))

(define make-BST
	(lambda ()
		(let ([bst '()])
			(lambda (msg . agrs)
				(case msg
					[(empty?) (empty-BST bst)]
					[(insert) (set! bst (BST-insert (car agrs) bst))]
					[(remove) (set! bst (BST-remove (car agrs) bst))]
					[(inorder) (BST-inorder bst)]
					[(preorder) (BST-preorder bst)]
					[(height) (BST-height bst)]
					[(contains?) (BST-contains? bst (car agrs))]
					[else (error 'msg "illegal message to BST")]
				)
			)
		)
	)
)
