(load "ch02_do_it_again.ss")
(load "ch03_cons_the_magnificent.ss")

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(set? '(apple peaches apple plum))
(set? '(apples peaches pears plums))
(set? '())

(set? '(apple 3 pear 4 9 apple 3 4))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))
                   
(makeset '(apple peach pear peach plum apple lemon peach))
(makeset '(apple 3 pear 4 9 apple 3 4))

(define makesetm
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cons (car lat) (makesetm (multirember (car lat) (cdr lat))))))))

(makesetm '(apple peach pear peach plum apple lemon peach))
(makesetm '(apple 3 pear 4 9 apple 3 4))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(subset? '(5 chicken wings) 
         '(5 hamburgers 2 pieces fried chicken and light duckling wings))

(subset? '(4 pounds of horseradish)
         '(four pounds chicken and 5 ounces horseradish))

(define subsetb?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
            (subsetb? (cdr set1) set2))))))

(subsetb? '(5 chicken wings) 
         '(5 hamburgers 2 pieces fried chicken and light duckling wings))

(subsetb? '(4 pounds of horseradish)
         '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (set1 set2)
     (and (subset? set1 set2)(subset? set2 set1))))
      ;((subset? set1 set2)
      ;(subset? set2 set1))
      ;(else #f))))
  
(eqset? '(6 large chickens with wings)
        '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) (cdr set2)) #t)
      (else (intersect? (cdr set1) set2)))))

(define intersectb?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1)(cdr set2))
          (intersect? (cdr set1) set2))))))
      
(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))
(intersect? '() '(x y z))
(intersect? '(a) '(x y z))

(intersectb? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))
(intersectb? '() '(x y z))
(intersectb? '(a) '(x y z))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2) 
       (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))
 
(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(difference '(a b c) '(c d e))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) 
                       (intersectall (cdr l-set)))))))

(intersectall '((a b c)(c a d e)(e f g h a b)))
(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plumns)
                (and 6 prunes with some apples)))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))
      

(a-pair? '(pear pear))
(a-pair? '(3 7))
(a-pair? '((2) '(pair)))
(a-pair? 'a)
(a-pair? '(a b c))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(first '(a b c))
(second '(a b c))
(build '(a) '(b))
(build 'a 'b)
(third '(a b c))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((8 3)(4 2)(7 6)(6 2)(3 4)))
(fun? '((d 4)(b 0)(b 9)(e 5)(g 4)))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (build 
                   (second (car rel))
                   (first (car rel)))
                  (revrel (cdr rel)))))))

;not using help functions decreases readability
(define revrel2
  (lambda (rel)
    (cond
      ((null? rel)(quote()))
      (else (cons (cons
                   (car (cdr (car rel)))
                  (cons (car (car rel))
                        (quote())))
            (revrel2 (cdr rel)))))))
        
(revrel '((a 8)(pie pumpkin)(sick got)))
(revrel2 '((a 8)(pie pumpkin)(sick got)))

(define revpair
  (lambda (pair)
    (build (second pair)(first pair))))

(define revrel3
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

(revrel3 '((a 8)(pie pumpkin)(sick got)))

(define seconds
  (lambda (l)
    (cond
      ((null? l)(quote()))
      (else (cons (second (car l)) (seconds (cdr l)))))))
    
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((grape raisin)(plum prune)(stewed prune)))
(fullfun? '((grape raisin)(plum prune)(stewed grape)))
(seconds '((a b)(c d)))
           
                     