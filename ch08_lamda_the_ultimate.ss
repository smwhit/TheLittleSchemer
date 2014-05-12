(load "ch02_do_it_again.ss")
(load "ch03_cons_the_magnificent.ss")
(load "ch05_full_of_stars.ss")
 
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

(define rember-f
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(rember-f 'a '(a b c))

;(seqrem ('a 'b '(a b c)))


