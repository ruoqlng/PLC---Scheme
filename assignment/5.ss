; -Assignment 5 ouyangr
; #1 

(define (interval-intersects? i1 i2)
    (if (and (<= (list-ref i2 0) (list-ref i1 1)) (<= (list-ref i1 0) (list-ref i2 1)))
             #t
        #f))

(define (interval-union i1 i2)
    (if (interval-intersects? i1 i2)
        (list (min (list-ref i1 0) (list-ref i2 0)) (max (list-ref i1 1) (list-ref i2 1)))
        (list i1 i2)
        ))

(define (sort-helper x y)
    (cond
      [(< (car x) (car y)) #t]
      [(> (car x) (car y)) #f]
      [(< (cadr x) (cadr y)) #t]
      [else #f]))

(define (minimize-helper first rest)
  (cond
    [(null? rest) (list first)]
    [(interval-intersects? first (car rest)) 
       (minimize-helper (interval-union first (car rest)) (cdr rest))]
    [else (cons first (minimize-helper (car rest) (cdr rest)))]
  )
)

(define (minimize-interval-list ls)
    (minimize-helper (car (list-sort sort-helper ls)) (cdr (list-sort sort-helper ls)))
)

; #2
(define (exists? pred ls)
  (if (null? ls) #f 
    (or (pred (car ls)) (exists? pred (cdr ls)))
  )
)

; #3
(define (product s1 s2)
  (cond
    [(or (null? s1) (null? s2)) '()]
    [(append (map (lambda (x) (list (car s1) x)) s2) (product (cdr s1) s2))]
  )
)

; #4
(define (replace old new ls)
  (cond
    [(null? ls) '()]
    [(equal? old (car ls))
     (cons new (replace old new (cdr ls)))]
    [else (cons (car ls) (replace old new (cdr ls)))]
  )
)

; #5
(define (remove-last element ls)
  (cond
    [(null? ls) '()]
    [(and (equal? (car ls) element) (not (member element (cdr ls))))
       (cdr ls)]
    [else (cons (car ls) (remove-last element (cdr ls)))]
  )
)