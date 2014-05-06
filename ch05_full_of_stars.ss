;Commandment #1
;when recurring on a list of atoms, ask null? l and else
;when recurring on a number, ask zero? and else
;when recurring in a list of S-expressions l, ask null?, (atom? (car l)) and else

(load "ch01_toys.ss")
(load "ch04_numbers.ss")

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l) 
                     (rember* a (cdr l))))))
       (else (cons (rember* a (car l))
                   (rember* a (cdr l)))))))


(rember* 'a '(a (a ((a c)) b)))
(rember* 'a '(x y z))
                           
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      (atom? (car l)
       (cond
         ((eq? (car l) old)
          (cons old (cons new 
                      (insertR* new old (cdr l)))))
         (else (cons old 
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) 
                  (insertR* new old (cdr l)))))))
         
(insertR* 'new 'old '(old old))

(define occurs* 
  (lambda (a l)
    (cond
      ((null? l) 0)
      (atom? (car l)
             (cond
               ((eq? a (car l)) (add1 (occurs* a (cdr l))))
               (else (occurs* a (cdr l)))))
      (else (o+ (occurs* a (car l)) (occurs* a (cdr l)))))))

(occurs* 'hello '(hello helo hello))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      (atom? (car l)
             (cond
               ((eq? old (car l)) (cons new (subst* new old (cdr l))))
               (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'hello 'goodbye '(goodbye friends))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      (atom? (car l)
             (cond
               ((eq? old (car l)) 
                     (cons new (cons old (insertL* new old (cdr l)))))
               (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* 'hi 'bye '(bye everyone bye bye))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (or (eq? a (car l)) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))


(member* 'a '(a b c))        

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost '((potato) (chips ((with) fish)(chips))))
(leftmost '(((hot)(tuna(and)))cheese))
(leftmost '(((() four)) 17 (seventeen)))
(leftmost '())

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2)) #t)
      ;((and (null? l1) (atom? (car l2))) #f)
      ;((null? l1) #f)
      ;((and (atom? (car l1)) (null? l2)) #f)
      ((or (null? l1)(null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
       (and (eqlist? (car l1)(car l2))
            (eqlist? (cdr l1)(cdr l2)))))))
     
(eqlist? '(strawberry ice cream) '(strawberry ice cream))
(eqlist? '(strawberry ice cream) '(strawberry cream ice))
(eqlist? '(banana ((split))) '((banana) (split)))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))      

(define equalEx?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1)(atom? s2)) 
       (eqan? s1 s2))
      ((atom? s1) #f)
      ((atom? s2) #f)
      (else (eqlist? s1 s2)))))

(equalEx? 1 2)
(equalEx? 1 1)
(equalEx? '(1) '(1))
(equalEx? '(1 2 3) '(4 5 6))

(define rember2
  (lambda (s l)
    (cond
      ((null? l)(quote()))
      ((equalEx? (car l) s)(cdr l))
      (else (cons (car l)
                  (rember2 s (cdr l)))))))
                           
