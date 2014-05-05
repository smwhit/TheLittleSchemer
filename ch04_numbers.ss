(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))


(define o+
    (lambda (n m)
      (cond
        ((zero? m) n)
        (else (add1 (o+ n (sub1 m)))))))

(add1 67)
(add1 0)
(sub1 67)
(sub1 0)
(add1 (sub1 67))

(zero? 0) #t
(zero? 1) #f

(o+ 46 12)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(o- 12 3)
(o- 12 0)
;(o- 999999 9989999) ;; this produces "The program ran out of memory message"
(o- 18 25)

;;tuples
'(2 11 3 79 47 6) ;a tuple
'(1 2 8 apple 4 3) ;not a tuple, it's a list of atoms
'() ; is a tuple of zero numbers, the empty tup

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      ((o+ (car tup) (addtup (cdr tup)))))))

(addtup '(3 5 2 8))
(addtup '(15 6 7 12 3))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(x 3 4)
(x 10 10)
(x 0 0)

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      ((add1 (o+ n (sub1 m)))))))

(o+ 10 20)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      ((sub1 (o- n (sub1 m)))))))

(o- 10 5)

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (( o+ n (x n (sub1 m)))))))

(x 2 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)                          
      ((cons (o+ (car tup1) (car tup2))
             (tup+
              (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2 3) '(2 1 0))

(define o>
  (lambda (n m)
    (cond
      ;((eq? m n) #f)
      ((zero? n) #f)  
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(o> 3 4)
(o> 4 3)
(o> 3 3)

(o< 3 4)
(o< 4 3)
(o< 3 3)

(define o=
  (lambda (m n)
    (cond
      ((o< m n) #f)
      ((o> m n) #f)
      (else #t))))
         
(o= 3 3)
(o= 3 4)
(o= 4 3)

(define expx
  (lambda (n e)
    (cond
      ((zero? e) 1)
      (else (x n (expx n (sub1 e)))))))
            
(expx 2 4)

(define div
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (div ( o- n m) m))))))
                                      
(div 8 3)
(div 15 4)

(define lengthx
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (lengthx (cdr lat)))))))

(lengthx '(1 2 3 4))
                               
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      ((pick (sub1 n) (cdr lat))))))

(pick 1 '(a b c))
(pick 2 '(a b c))
(pick 3 '(a b c))
                                               
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      ((cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 2 '(1 2 3))
                                               
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      ((cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(a 1 2 ))
                                               
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(a 1 2 3 b))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1)(number? a2)) 
       (o= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

(eqan? 'a 'a)
(eqan? 'a 'b)
(eqan? 'a 1)
(eqan? 0 0)
 
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? a (car lat)) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))


(occur 'a '(a a a))
(occur 'b '(a a a))

(define one?
  (lambda (n)
    (cond
      ((eqan? n 1) #t)
      (else #f))))

(one? 1)
(one? 2)
(one? 0)
       
       