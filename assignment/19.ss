; CSSE 304:  A19

; IN THIS FILE:
; Starting-code - CPS with continuations represented by Scheme procs
; Tests for starting code
; Tests for code that has been converted to Data Structure conts.
; Tests for code in imperative-form.
; Trace-it code - may be useful for debugging imperative-form code.


; NOTES ABOUT THE ASSIGNMENT
; Requirements for imperative form:
;  These procedures in CPS (all are thunks - take no arguments):
;    append-cps, cons-cps, +-cps, apply-k, flatten-cps,
;    list-sum-cps, helper procedure in cps-snlist-recur,
;  the procedures that are arguments to cps-snlist-recur.
;  You must define these global variables:  slist, k
;    The test code sets those variables in each test case. 
;    You may want the other variables used by imperative form
;    to also be global.

; You are allowed to go straight to imperative form without
;   first doing the intermediate DS-continuations form,
;   but I do not recommend it.

;;--------------- Starting code ----------------
(define scheme-value? (lambda (v) #t))

(define-datatype continuation continuation? 
  [init-k]
  [list-k]
  [id-k]
  [length-k]
  [cps-ls-cdr-recur 
    (proc scheme-value?) 
    (ls scheme-value?) 
    (k1 continuation?)]
  [cps-ls-car-recur 
    (proc scheme-value?) 
    (ls scheme-value?) 
    (helper scheme-value?) 
    (k1 continuation?)]
  [append-k 
    (a scheme-value?)
    (k1 continuation?)])

(define apply-k
  (lambda ()
    (cases continuation k
      [init-k () v]
      [list-k () (list v)]
      [id-k () v]
      [length-k () 
        (length v)]
      [cps-ls-cdr-recur (proc ls k1)
        (begin
          (set! y ls)
          (set! x v)
          (set! k k1)
          (proc))]
      [cps-ls-car-recur (proc ls helper k1)
        (begin
          (set! slist (cdr ls))
          (set! k (cps-ls-cdr-recur proc v k1))
          (helper))]
      [append-k (y k1) 
        (begin
          (set! k k1)
          (set! v (cons (car y) v))
          (apply-k))])))

(define make-k
  (lambda (k) k))
	
(define identity (lambda (v) v))

(define cps-snlist-recur
   (lambda (base car-item-proc-cps car-list-proc-cps)
   	(letrec
   		([helper
   			(lambda ()
   				(if (null? slist)
   					(begin
   						(set! v base)
   						(apply-k))
   					(if (not (or (null? (car slist)) (pair? (car slist))))
   						(begin
   							(set! k (cps-ls-cdr-recur car-item-proc-cps (car slist) k))
   							(set! slist (cdr slist))
   							(helper))
   						(begin
   							(set! k
   								(cps-ls-car-recur car-list-proc-cps slist helper k))
   							(set! slist (car slist))
   							(helper)))))])
   		helper)))


(define append-cps
  (lambda ()
    (if (null? y)
      (begin 
        (set! v x)
        (apply-k))
      (begin
        (set! k (append-k y k))
        (set! y (cdr y))
        (append-cps)))))

(define cons-cps
  (lambda ()
    (set! v (cons y x))
    (apply-k)))

(define +-cps
  (lambda ()
    (set! v (+ x y))
    (apply-k)))

(define flatten-cps
  (cps-snlist-recur '() cons-cps append-cps))

(define sum-cps
  (cps-snlist-recur 0 +-cps +-cps))
