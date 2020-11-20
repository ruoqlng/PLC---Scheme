 ; Assignment 9 ouyangr
; #1
(define snlist-recur
  (lambda (base-val ls-proc nls-proc)
    (letrec ([snlrec
        	      (lambda (slist)
		(if (null? slist) base-val
		    (if (list? (car slist))
			(ls-proc (snlrec (car slist)) (snlrec (cdr slist)))
			(nls-proc (car slist) (snlrec (cdr slist))))))])
      snlrec)))

;a
(define sn-list-sum
  (snlist-recur 0 + +))

;b
(define sn-list-map
  (lambda (proc slist)
    ((snlist-recur '() cons (lambda (x y)
			      (cons (proc x) y)))
     slist)))

;c
(define sn-list-paren-count
  (snlist-recur 2
		+
		(lambda (x y) y)))

;d
(define sn-list-reverse
  (snlist-recur '()
		(lambda (x y)
		  (append y (list x)))
		(lambda (x y)
		  (append y (list x)))))

;e
(define sn-list-occur
  (lambda (s slist)
    ((snlist-recur 0
		   +
		   (lambda (x y)
		     (if (eq? x s)
			 (add1 y)
			 y)))
     slist)))

;f
(define sn-list-depth
  (snlist-recur 1
		(lambda (x y)
		  (if (< x y)
		      y
		      (add1 x)))
		(lambda (x y)
		  y)))

; #2
(define bt-recur
  (lambda (base-val num-proc sym-proc)
    (letrec ([btrec
	      (lambda (bt)
		(if (null? bt) base-val
		    (if (number? bt) (num-proc bt)
			(sym-proc (car bt)
				  (btrec (cadr bt)) (btrec (caddr bt))))))])
      btrec)))

(define bt-sum
  (bt-recur 0
	    +
	    (lambda (x y z)
	      (+ y z))))

(define bt-inorder
  (bt-recur '()
	    (lambda (x) 
	      '())
	    (lambda (x y z)
	      (append y (list x) z))))