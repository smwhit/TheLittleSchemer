(load "ch08_lambda_the_ultimate.ss")

(define a-friend
  (lambda (x y)
    (null? y)))

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

;(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)