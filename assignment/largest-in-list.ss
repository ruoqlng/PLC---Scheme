(define largest-in-lists
  (lambda (ls)
    (cond [(null? ls) #f]
          [(null? (cdr ls) (car ls))]
          [else (max (car ls) (largest-in-lists (cdr ls)))])))