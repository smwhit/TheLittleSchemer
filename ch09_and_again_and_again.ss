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
  (lambda (x n)
    (display n)
    (newline)
    (eternity x (+ 1 n))))

;(eternity 'a 0)

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair)) (second pair)))))

(shift '((a b) c))
(shift '((a b)(c d)))