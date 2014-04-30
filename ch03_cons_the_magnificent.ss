(define naive_rembr
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
             ((eq? (car lat) a) (cdr lat))
             (else (naive_rembr a
                           (cdr lat))))))))

(naive_rembr 'bacon '(bacon lettuce and tomato))
(naive_rembr 'and '(bacon lettuce and tomato)) ;should be bacon lettuce tomato
(naive_rembr 'x '())

(define rembr
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rembr a (cdr lat)))))))

(rembr 'bacon '(bacon lettuce and tomato))
(rembr 'and '(bacon lettuce and tomato))
(rembr 'xyz '())

;; my own function to remove all occurrences of an atom
(define rembra
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) 
            (rembra a (cdr lat)))
      (else (cons (car lat)
                  (rembr a (cdr lat)))))))

(rembra 'a '(a a b c a d)) ;; should be (b c d)
(rembra 'a '()) ; ()
(rembra 'a '(a)) ; ()

(car (car '((apple peach pumpkin) (x y z))))

(car (car (cdr '((apple peach pumpkin) (x y z)))))