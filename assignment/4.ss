; -Assignment 4 ouyangr

; #1
(define (matrix-ref m row col)
  (list-ref (list-ref m row) col)
)

; #2
(define (matrix? obj)
  (if (not (list? obj)) #f
    (if (null? (cdr obj)) #t
      (let ([check-number (lambda (l)
                            (and (andmap number? (car l))
                                   (andmap number? (cadr l))))])
          (if(and (list? (car obj)) (check-number obj)
                (not (eq? (length (car obj)) 0))
                (eq? (length (car obj)) (length (cadr obj))))
            (matrix? (cdr obj)) #f)
      )
    )
  )
)

; #3
(define (matrix-transpose m)
  (cond
    [(null? (car m)) '()]
    [else (cons (map car m) (matrix-transpose (map cdr m)))]
  )
)

; #4
(define (filter-in pred? ls)
  (cond
    [(null? ls) '()]
    [(pred? (car ls))
      (cons (car ls) (filter-in pred? (cdr ls)))]
    [else (filter-in pred? (cdr ls))]
  )
)

; #5
(define (invert ls)
  (cond
    [(null? ls) '()]
    [else (cons (list (cadr (car ls)) (car (car ls)))
     (invert (cdr ls)))]
  )
)


; #6
(define (pascal-triangle-helper ls)
  (if (equal? 1 (length ls)) '()
    (cons (+ (car ls) (cadr ls)) (pascal-triangle-helper (cdr ls)))
  )
)

(define (pascal-next ls)
    (append (cons '1 (pascal-triangle-helper (car ls))) '(1))
)

(define (pascal-triangle n)
  (cond
     [(negative? n) '()]
     [(zero? n) '((1))]
     [else (cons (pascal-next (pascal-triangle (- n 1)))
     (pascal-triangle (- n 1)))]
  )
)