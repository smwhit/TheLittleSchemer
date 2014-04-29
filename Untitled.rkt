(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define lat?
  (lambda (l)
         (cond
           ((null? l) #t)
           ((atom? (car l)) (lat? (cdr l)))
           (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))
;(atom? 'a)

(define naive_rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
             ((eq? (car lat) a) (cdr lat))
             (else (naive_rember a
                           (cdr lat))))))))

(define rembr
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rembr a (cdr lat)))))))

(member? 'meat '(potatoes and meat gravy))