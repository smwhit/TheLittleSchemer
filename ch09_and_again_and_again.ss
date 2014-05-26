(load "ch04_numbers.ss")
(load "ch07_friends_and_relations.ss")

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? sorn a)))))

(looking 'a '(a b c))
(looking 'a '(b c d))
(looking 'caviar '(6 2 4 caviar 5 7 3))
;(looking 10 '(1 2 3 4)) ; never finishes
;(looking 'caviar '(7 1 2 caviar 5 6 3))
; These are called partual functions
; Functions in the book previously have been 'total' functions

(define eternity
  (lambda (x)
    (eternity x)))

(define eternity2
  (lambda (x n)
    (display n)
    (newline)
    (eternity2 x (+ 1 n))))

;(eternity 'a 0)

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair)) (second pair)))))

(shift '((a b) c))
(shift '((a b)(c d)))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora) ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else 
       (+ (length* (first pora))
          (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (* (weight* (first pora)) 2)
          (weight* (second pora)))))))

(weight* '((a b) c))
(weight* '(a (b c)))
;weight looks at all arguments of a pair, it is total

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
                (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(shuffle '(a (b c)))
(shuffle '(a b))
;(shuffle '((a b)(c d))) ;infinite!

;http://en.wikipedia.org/wiki/Collatz_conjecture
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (/ n 2)))
         (else (C (add1 (* 3 n)))))))))
       
(C 10)

;http://en.wikipedia.org/wiki/Ackermann_function
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

(A 0 0)
(A 1 0)
(A 1 1)
(A 2 2)
;(A 3 3)
;(A 4 3)

(define will-stop?
  (lambda (f)
    ;... ;cannot define
    (write 'impossible)))


((lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l)))))) '())

((lambda (l)
  (cond
    ((null? l) 0)
    (else 
     (add1 
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else (add1
                  (eternity (cdr l))))))
         (cdr l)))))) '(1))
            
((lambda (l)
  (cond
    ((null? l) 0)
    (else
     (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1
             ((lambda (l)
                (cond
                  ((null? l) 0)
                  (else
                   (add1 (eternity (cdr l))))))
              (cdr l))))))
       (cdr l)))))) '(1 2))

(((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity) '())

(((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
 eternity)) '(q))

(((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
     eternity))) '(1 2))
     
(((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))) '())

(((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))) '(a))

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length eternity))))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))) '(a b))

(((lambda (mk-length)
    (mk-length
     (mk-length
      (mk-length
       (mk-length eternity)))))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))) '(a b c))

(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
         (add1
           ((mk-length mk-length) (cdr l))))))))
 '(1 2 3 4 5))

;(((lambda (mk-length)
;   (mk-length mk-length))
; (lambda (mk-length)
;   ((lambda (length)
;      (lambda (l)
;        (cond 
;          ((null? l) 0)
;          (else (add1 (length (cdr l)))))))
;   (mk-length mk-length)))) '(apples))


;(((lambda (mk-length) 
;    (mk-length mk-length))
;  (lambda (mk-length) 
;    ((lambda (length)
;      (lambda (l) (cond
;                    ((null? l) 0)
;                    (else (addl (length (cdr l))))))) (mk-length mk-length))))
;'(apples))

(((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))) '(apples and pears))
;wot?

(write "hello")


      