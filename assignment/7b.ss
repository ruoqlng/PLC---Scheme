; -Assignment 7b ouyangr
; #5
;  a
(define (slist-map proc slist)
  (let map-proc ([slist slist])
    (cond
     [(null? slist) '()]
     [(symbol? (car slist))
      (cons (proc (car slist)) (map-proc (cdr slist)))]
     [else
      (cons (map-proc (car slist)) (map-proc (cdr slist)))])))
;  b
(define (slist-reverse slist)
  (let reverse-slist ([slist slist])
    (cond
     [(null? slist) '()]
     [(symbol? (car slist))
      (append (reverse-slist (cdr slist)) (list (car slist)))]
     [else
      (append (reverse-slist (cdr slist))
	      (list (reverse-slist (car slist))))])))
;  c
(define (slist-paren-count slist)
  (let count ([slist slist])
    (cond
     [(null? slist) 2]
     [(symbol? (car slist))
      (count (cdr slist))]
     [else
      (+ (count (car slist)) (count (cdr slist)))])))
;  d
(define (slist-depth slist)
  (let get-depth ([slist slist])
    (cond
     [(null? slist) 1]
     [(symbol? (car slist))
      (get-depth (cdr slist))]
     [else 
      (max (add1 (get-depth (car slist))) (get-depth (cdr slist)))])))
;  e
(define (slist-symbols-at-depth slist d)
  (let get-symbols ([slist slist]
		    [depth 1])
    (cond
     [(null? slist) '()]
     [(symbol? (car slist))
      (if (= d depth)
	  (append (list (car slist)) (get-symbols (cdr slist) depth))
	  (get-symbols (cdr slist) depth))]
     [else
      (append (get-symbols (car slist) (add1 depth)) (get-symbols (cdr slist) depth))])))
; #6
(define path-to
  (lambda (ls sym)
    (cond
     [(null? ls) #f]
     [(list? (car ls)) (letrec ([x (path-to (car ls) sym)]
				[y (path-to (cdr ls) sym)])
			 (cond
			  [(and (not x) (not y)) #f]
			  [(not x) (append (list 'cdr) y)]
			  [(not y) (append (list 'car) x)]))]
     [(eq? sym (car ls)) (list 'car)]
     [else (letrec ([y (path-to (cdr ls) sym)])
	     (cond
	      [(not y) #f]
	      [else (append (list 'cdr) y)]))])))
; #7
(define make-c...r
  (lambda (ls)
    (lambda (x)
      ((make-helper (string->list ls)) x))))
(define make-helper 
  (lambda (ls)
    (cond
     [(null? ls) (lambda (x) x)]
     [(equal? (car ls) '#\d) (lambda (x) (cdr ((make-helper (cdr ls)) x)))]
     [(equal? (car ls) '#\a) (lambda (x) (car ((make-helper (cdr ls)) x)))])))