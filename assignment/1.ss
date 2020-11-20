; a1 ouyangr
; #1
(define interval-contains?
  (lambda (interval num)
    (if (<  num (car interval))
	#f
	(if (>  num (cadr interval))
	    #f
	    #t))))
; #2
(define interval-intersects? 
  (lambda (int1 int2)
    (if (interval-contains? int1 (car int2))
	#t
	(if (interval-contains? int2 (car int1))
	    #t
	    #f))))
; #3
(define interval-union
  (lambda (int1 int2)
    (if (not (interval-intersects? int1 int2))
	(list int1 int2)
        (list (list (min (car int1) (car int2)) (max (cadr int1) (cadr int2)))))))
; #4
(define first
	(lambda (int)
		(car int)))
(define second
	(lambda (int)
		(cadr int)))
(define third
    (lambda (int)
		(caddr int)))

; #5
(define make-vec-from-points
  	(lambda (vec1 vec2)
    	(list(- (first vec2) (first vec1)) (- (second vec2) (second vec1)) (- (third vec2) (third vec1)))))

; #6
(define dot-product
  	(lambda (vec1 vec2)
    	(+ (* (first vec2) (first vec1)) (* (second vec2) (second vec1)) (* (third vec2) (third vec1)))))

; #7
(define square
  	(lambda (num)
    	(* num num)))
(define vector-magnitude
	(lambda (vec)
		(sqrt (+ (square (first vec)) (square (second vec)) (square (third vec))))))

; #8
(define distance
	(lambda (vec1 vec2)
		(sqrt (+ (square (- (first vec2) (first vec1))) 
	    	(square (- (second vec2) (second vec1)))
	    	(square (- (third vec2) (third vec1)))))))
