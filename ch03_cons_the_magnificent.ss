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
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) 
            (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(multirember 'a '(a a b c a d)) ;; should be (b c d)
(multirember 'a '()) ; ()
(multirember 'a '(a)) ; ()
(multirember 'a '(x y z))

(car (car '((apple peach pumpkin) (x y z))))

(car (car (cdr '((apple peach pumpkin) (x y z)))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))
(firsts '((a b) (c d) (e f)))

(firsts '())

(firsts '(((five plums) four)
          (eleven green oranges)
          ((no) more)))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'jalapeno 'and '(tacos tamales and salsa))
(insertR 'e 'd '(a b c d f g h))
(insertR 'a 'b '())
(insertR 'hello 'and '(goodbye everyone))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (cdr lat))))))

(insertL 'l 'r '(r))


(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'hello 'jello '(jello nice to meet you))
(subst 'hello 'jello '(jello jello))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      ((or (eq? o1 (car lat))(eq? o2 (car lat))) (cons new (cdr lat)))
      ;((eq? new o1) (cons new (cdr lat)))
      ;((eq? new o2) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 'a 'b 'c '(b d))
(subst2 'a 'b 'c '(c d))  

(define multiinsertR 
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old 
             (cons new 
                   (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) 
                  (multiinsertR new old (cdr lat)))))))

(multiinsertR 'b 'a '(a a b c))
(multiinsertR 'a 'a '(a a))
(multiinsertR 'a 'b '())
(multiinsertR 'g '_ '(a b c d e f _ ))