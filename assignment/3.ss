; -Assignment 3 ouyangr

; #1
(define intersection 
  (lambda (l1 l2)
    (cond
      [(or (null? l1) (null? l2)) '()]
      [(member (car l1) l2) (cons (car l1) (intersection (cdr l1) l2))]
      [else (intersection (cdr l1) l2)]
    )
  )
)

; #2
(define subset?
  (lambda (l1 l2)
    (cond
      [(null? l1) #t]
      [(member (car l1) l2) (subset? (cdr l1) l2)]
      [else #f]
    )
  )
)

; #3
(define set?
  (lambda (ls)
    (cond
      [(null? ls) #t]
      [(member (car ls) (cdr ls)) #f]
      [else (set? (cdr ls))]
    )
  )
)

(define relation?
  (lambda (l)
    (cond
      [(null? l) #t]
      [(not (list? l)) #f]
      [(not (set? l)) #f]
      [(not (list? (car l))) #f]
      [(not (null? (cddar l))) #f]
      [else (relation? (cdr l))]
    )
  )
)

; #4
(define domain
  (lambda (l)
    (cond
     [(null? l) '()]
     [(member (caar l) (domain (cdr l))) (domain (cdr l))]
     [else (cons (caar l) (domain (cdr l)))]
    )
  )
)

; #5
(define union
  (lambda (l1 l2)    
    (cond 
      [(null? l1) l2]
      [(member (car l1) l2) (union (cdr l1) l2)]
      [else (union (cdr l1) (cons (car l1) l2))]
    )
  )
)

(define range
  (lambda (l)
    (cond
      [(null? l) '()]
      [(member (cadar l) (range (cdr l))) (range (cdr l))]
      [else (cons (cadar l) (range (cdr l)))]
    )
  )
)
	
(define helper
  (lambda (l) 
    (union (range l) (domain l))
  )
)

(define helper1
  (lambda (l l1) 
    (if (null? (cdr l)) 
      (cons (list (car l)(car l)) l1)
      (helper1 (cdr l)(cons (list (car l)(car l)) l1))
    )
  )
)
	
(define reflexive?
  (lambda (l)
    (if (null? l) 
      #t
       (subset? (helper1 (helper l) '()) l)
    )
  )
)

; #6
(define check
  (lambda (r)
    (cond
      [(null? r) #t]
      [(not (symbol? (car r))) #f]
      [(not (integer? (cadr r))) #f]
      [(not (positive? (cadr r))) #f]
      [else #t]
    )
  )
)

(define multi-set?
  (lambda (obj)
    (cond
      [(not (relation? obj)) #f]
      [(member #f (map check obj)) #f]
      [else (set? (map car obj))]
    )
  )
)

; #8
(define ms-size
  (lambda (l)
    (apply + (map cadr l))
  )
)

; #9
(define last
  (lambda (l)
    (cond 
      [(null? (cdr l)) (car l)]
      [else (last (cdr l))]
    )
  )
)

; #10
(define all-but-last
  (lambda (l)
    (cond
      [(null? (cdr l)) '()]
      [else (cons (car l) (all-but-last (cdr l)))]
    )
  )
)