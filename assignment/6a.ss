; - Assignment 6a ouyangr

; #1
(define curry2
  (lambda (proc) (lambda (x) (lambda (y) 
			       (proc x y)))))
; #2
(define curried-compose
  (lambda (proc) (lambda (proc1) (lambda (x)
				   (proc (proc1 x))))))
; #3
(define compose
  (lambda list-of-functions
	 (if (null? (cdr list-of-functions)) (car list-of-functions)
	     (lambda (l) ((car list-of-functions)
			   ((apply compose (cdr list-of-functions)) l))))))
; #4
(define make-list-c
  (lambda (n)
    (if (zero? n)
	(lambda (ls)
	  '())
	(lambda (ls)
	  (cons ls ((make-list-c (sub1 n)) ls))))))
; #5
(define reverse-it
  (letrec ([reverse-helper
	    (lambda (l1 l2)
	      (if (null? l1) l2
		  (reverse-helper (cdr l1) (cons (car l1) l2))))])
    (lambda (l3)
      (reverse-helper l3 '()))))
; #6
(define map-by-position
  (lambda (fn-list arg-list)
    (map (lambda (x y) (x y)) fn-list arg-list)))
; #7
(define empty-BST (lambda ()'()))
(define empty-BST? (lambda (obj) (null? obj)))
(define BST-insert
  (lambda (num bst)
    (if (empty-BST? bst)
	(list num '() '())
	(cond
	 [(= (car bst) num) bst]
	 [(> (car bst) num) 
	  (list (car bst) (BST-insert num (cadr bst)) (caddr bst))]
	 [(< (car bst) num)
	  (list (car bst) (cadr bst) (BST-insert num (caddr bst)))]))))
(define BST-inorder
  (lambda (bst)
    (if (empty-BST? bst)
	'()
	(append (BST-inorder (cadr bst)) (list (car bst))
		(BST-inorder (caddr bst))))))
(define BST?
  (lambda (obj)
    (cond 
     [(null? obj) #t]
     [(not (list? obj)) #f]
     [(not (= (length obj) 3)) #f]
     [(not (number? (car obj))) #f]
     [(not (list? (cadr obj))) #f]
     [(not (list? (caddr obj))) #f]
     [(not (apply < (BST-inorder obj))) #f]
     [else (and (BST? (cadr obj)) (BST? (caddr obj)))])))
(define BST-element (lambda (node) (car node)))
(define BST-left (lambda (node) (cadr node)))
(define BST-right (lambda (node) (caddr node)))
(define BST-insert-nodes
  (lambda (bst nums)
    (if (null? nums)
	bst
	(BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))
(define BST-contains?
  (lambda (bst num)
    (cond
     [(empty-BST? bst) #f]
     [(= (car bst) num) #t]
     [(> (car bst) num) (BST-contains? (BST-left bst) num)]
     [(< (car bst) num) (BST-contains? (BST-right bst) num)])))
(define BST-height
  (lambda (bst)
    (if (null? bst)
	-1
	(add1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst)))))))