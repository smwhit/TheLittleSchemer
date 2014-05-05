;Commandment #1
;when recurring on a list of atoms, ask null? l and else
;when recurring on a number, ask zero? and else
;when recurring in a list of S-expressions l, ask null?, (atom? (car l)) and else

(load "ch01_toys.ss")

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l) 
                     (rember* a (cdr l))))))
       (else (cons (rember* a (car l))
                   (rember* a (cdr l)))))))


(rember* 'a '(a (a ((a c)) b)))
(rember* 'a '(x y z))
                           
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      (atom? (car l)
       (cond
         ((eq? (car l) old)
          (cons old (cons new 
                      (insertR* new old (cdr l)))))
         (else (cons old 
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) 
                  (insertR* new old (cdr l)))))))
         
(insertR* 'new 'old '(old old))

(define occurs* 
  (lambda (a l)
    (cond
      ((null? l) 0)
      (atom? (car l)
             (cond
               ((eq? a (car l) 
                     (add1 (occurs* a (cdr l)))))
               (else (occurs* a (cdr l)))))
      (else (o+ (occurs* a (car l)) (occurs* a (cdr l)))))))
  
