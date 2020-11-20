; - Assignment 7a ouyangr
; #1
(define lst-append
  (lambda (nv l set i)
    (if (null? l) 
	nv 
	(and (vector-set! nv (+ i set) (car l))
	     (lst-append nv (cdr l) set (+ 1 i))))))
(define vec-append
  (lambda (nv vec i)
    (if (< i (vector-length vec))
	(and (vector-set! nv i (vector-ref vec i))
	     (vec-append nv vec (+ 1 i)))
	nv)))			
(define vector-append-list
  (lambda (v lst)
    (lst-append 
     (vec-append 
      (make-vector (+ (vector-length v)(length lst)))  v  '0 ) 
     lst 
     (vector-length v) '0)))
; #2
(define group-by-two
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [(null? (cdr ls)) (list ls)]
     [else (cons (list (car ls) (cadr ls)) (group-by-two (cddr ls)))])))
; #3
(define get-rest-groups
  (lambda (ls n)
    (if (= n 0)
	ls
	(get-rest-groups (cdr ls) (sub1 n)))))
(define group-by-n
  (lambda (ls n)
    (letrec ([get-first-group
	      (lambda (ls n)
		(if (= n 0)
		    '()
		    (cons (car ls) (get-first-group (cdr ls) (sub1 n)))))])
      (cond
       [(null? ls) ls]
       [(< (length ls) n) (list ls)]
       [else (cons (get-first-group ls n)
		   (group-by-n (get-rest-groups ls n) n))]))))
; #4
(define bt-leaf-sum
  (lambda (T)
    (if (number? T)
	T
	(+ (bt-leaf-sum (cadr T)) (bt-leaf-sum (caddr T))))))
(define bt-inorder-list
  (lambda (T)
    (if (number? T)
        '()
        (append (bt-inorder-list (cadr T))
		(list (car T)) (bt-inorder-list (caddr T))))))
(define bt-max
  (lambda (T)
    (if (number? T)
	T
	(max (bt-max (cadr T)) (bt-max (caddr T))))))
(define left
  (lambda (s n ls)
    (if (< (cadr ls) (+ n (cadddr ls)))
        (list s (+ n (cadddr ls)) s (+ n (cadddr ls)))
        (list (car ls) (cadr ls) s (+ n (cadddr ls))))))
(define right
  (lambda (s n ls)
    (if (> (cadr ls) (+ n (cadddr ls)))
        (list (car ls) (cadr ls) s (+ n (cadddr ls)))
        (list s (+ n (cadddr ls)) s (+ n (cadddr ls))))))
(define bothl
  (lambda (s l1 l2)
    (if (>= (cadr l1) (cadr l2))
        (if (> (cadr l1) (+ (cadddr l1) (cadddr l2)))
            (list (car l1) (cadr l1) s (+ (cadddr l1) (cadddr l2)))
            (list s (+ (cadddr l1) (cadddr l2)) s (+ (cadddr l1) (cadddr l2))))
        (if (< (cadr l2) (+ (cadddr l1) (cadddr l2)))
            (list s (+ (cadddr l1) (cadddr l2)) s (+ (cadddr l1) (cadddr l2)))
            (list (car l2) (cadr l2) s (+ (cadddr l1) (cadddr l2)))))))
(define bt-max-interior-helper
  (lambda (T)
    (cond
     [(and (number? (cadr T)) (number? (caddr T))) 
      (list (car T) (+ (cadr T) (caddr T)) (car T) (+ (cadr T) (caddr T)))]
     [(and (number? (cadr T)) (list? (caddr T)))
      (right (car T) (cadr T) (bt-max-interior-helper (caddr T)))]
     [(and (list? (cadr T)) (number? (caddr T)))
      (left (car T) (caddr T) (bt-max-interior-helper (cadr T)))]
     [(and (list? (cadr T)) (list? (caddr T)))
      (bothl (car T) (bt-max-interior-helper (cadr T)) (bt-max-interior-helper (caddr T)))])))
(define bt-max-interior
  (lambda (T)
    (car (bt-max-interior-helper T))))