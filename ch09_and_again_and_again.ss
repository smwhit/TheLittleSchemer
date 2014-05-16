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
;(shuffle '((a b)(c d)))
       


      