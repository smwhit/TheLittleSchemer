(load "ch01_toys.ss")

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? '(bacon and eggs))
(lat? '(bacon (and eggs)))

(or (null? '()) (atom? '(d e f g)))
(or (null? '(a b c)) (null? '()))
(or (null? '(a b c)) (null? '(atom)))


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; member2 is my implementation of member.
; Personally prefer it as the the boolean expression is lost
; Would assume that the reason the authors included the original definition
; is to show the or statement in action
(define member2?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member2? a (cdr lat))))))

(member2? 'meat '(mashed potatoes and meat gravy))
(member? 'meat '(mashed potatoes and meat gravy))
(member? 'carrots '(mashed potatoes and meat gravy))
