(load "ch01_toys.ss")
(load "ch04_numbers.ss")
;arithmetic expressions
1
3

(quote +) ; atom +, not operation
(quote x)

;(define numbered?
;  (lambda (aexp)
;    (cond
;      ((atom? aexp) (number? aexp))
;      ((eq? (car (cdr aexp)) (quote +))
;        (and (numbered? (car aexp))(numbered? (car (cdr (cdr aexp))))))
;      ((eq? (car (cdr aexp)) (quote x))
;       (and (numbered? (car aexp))(numbered? (car (cdr (cdr aexp)))))))))
 (define numbered?
   (lambda (aexp)
     (cond
       ((atom? aexp)(number? aexp))
       (else
        (and (numbered? (car aexp))(numbered? (car (cdr (cdr aexp)))))))))

(numbered? '(1 x 2))
(numbered? '(cheese ppp 2))
(numbered? '(1 ! 4))
      
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp))(quote +)) 
       (+ (value (car nexp))(value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp))(quote x)) 
       (* (value (car nexp))(value (car (cdr (cdr nexp))))))
      (else (expx (value (car nexp))(value (car (cdr (cdr nexp)))))))))

(value '(1 + 2))
(value '(1 + (2 + 4)))
(value '(1 + (2 x 3)))
(value '(1 + (3 x (3 x 3))))
(value '(2 x (3 + 3)))
(value '(2 p 4))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
    
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define prefix-value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (prefix-value (1st-sub-exp nexp))
                        (prefix-value (2nd-sub-exp nexp))))
      ((eq? (operator nexp)(quote x))
       (* (prefix-value (1st-sub-exp aexp))
                        (prefix-value (2nd-sub-exp nexp))))
      (else 
       (expx (prefix-value (1st-sub-exp nexp))
             (prefix-value (2nd-sub-exp nexp)))))))

(prefix-value (+ 1 3))
(prefix-value (x 2 3))
(prefix-value (x 2 (+ 4 5)))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define lplus
  (lambda (n m)
    (cond 
      ((sero? m) n)
      (else (edd1 (lplus n (zub1 m)))))))

(define lplus2 
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (lplus2 n (zub1 m)))))))

(lplus '() '(()()))
(lplus2 '() '(()()))
              
       