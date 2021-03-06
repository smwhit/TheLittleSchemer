(load "ch02_do_it_again.ss")
(load "ch03_cons_the_magnificent.ss")
(load "ch05_full_of_stars.ss")
(load "ch06_shadows.ss")

(define rember-f
  (lambda (test?)
    (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((test? a (car l)) (cdr l))
      (else (cons (car l) ((rember-f test?) a (cdr l))))))))


((rember-f =) 5 '(6 2 5 3))
((rember-f eq?) 'jelly '(jelly beans are good))
((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
           (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(eq?-salad 'salat)

(eq?-salad 'salad)

((eq?-c 'salad) 'salad)

(define rember-eq? (rember-f eq?))

(rember-eq? 'tuna '(tuna salad is good))
((rember-f eq?) 'tuna '(tuna salad is good)) ;don't need to create a named function
(rember-eq? 'tuna '(shrimp salad and tuna salad))
((rember-f eq?) 'tuna '(shrimp salad and tuna salad))

;((rember-f eq?) eq? (equal? eq? eqan? eqlist?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? old (car l)) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? old (car l)) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

((insertL-f eq?) 'x 'a '(a b c))
((insertR-f eq?) 'x 'a '(a b c))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

((insert-g seqL) 'x 'a '(a b c))
((insert-g seqR) 'x 'a '(a b c))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(define insertL2
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(insertL 'x 'a '(a b c))
(insertR 'x 'a '(a b c))

(define insertR2
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(insertL2 'x 'a '(a b c))
(insertR2 'x 'a '(a b c))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst-f
  (insert-g seqS))

(subst-f 'bbb 'b '(a b c))

(define seqrem
  (lambda (new old l)
    l))

(define rember-f2
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(rember-f2 'a '(a b c))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) o+)
      ((eq? x (quote x)) *)
      (else expx))))

((atom-to-function '+) 2 3)
((atom-to-function 'x) 2 3)
((atom-to-function '^) 2 3)

(define value-f
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
            (value-f (1st-sub-exp nexp))
            (value-f (2nd-sub-exp nexp)))))))

(value-f '(+ 2 3))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote()))
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat)))))))) 

((multirember-f eq?) 'a '(a b c a b c))
((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multirember-eq 
  (multirember-f eq?))

(multirember-eq 'tuna '(shrimp salad tuna salad and tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote()))
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(define eq?-tuna
  (eq?-c 'tuna))

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col (quote()) (quote())))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat (cons (car lat) seen)))))
      (else
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)
(multirember&co 'cheese '(tuna) a-friend)
(multirember&co 'tuna '(tuna tuna) a-friend)
(multirember&co 'tuna '(cheese tuna) a-friend)

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat 
              (cons (quote tuna) seen))))

(multirember&co 'tuna '(tuna) new-friend)

(define last-friend
  (lambda (x y)
    (length x)))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons (quote tuna) newlat)
              seen)))

(multirember&co 'tuna '(and tuna) latest-friend)
(multirember&co 'tuna '(and) latest-friend)

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) oldL)
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(multiinsertLR 'insert 'a 'b '(a b c))