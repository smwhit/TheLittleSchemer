;;atoms

'atom
(quote atom)
'turkey
'1492
'u
'*abc$

;;lists
'(atom)
'(atom turkey or)
;'(atom turkey) 'or ;; not a list
'((atom turkey) or)

;;S-expressions
'(x y z)
'((x y) z)
'(how are you doing so far)
'(((how) are) ((you) (doing so)) far)

'() ; an empty list
'() ; not an atom
'(()()())

;;car
(car '(a b c)) ; head of the list is a
(car '((a b c) x y z))
;(car 'hotdog) ; cannot ask for the car of an atom
;(car '()) ; cannot ask for the car of an empty list

(car '(((hotdogs)) (and) (pickle) relish)) ; list of lists containing hotdog
(car (car '(((hotdogs))(and))))

(cdr '(a b c))
(cdr '((a b c) x y z))
(cdr '(hamburger))
(cdr '((x) t r))
;(cdr 'a) ; cannot ask for the cdr of an atom
;(cdr '()) ; cannot ask for the cdr of an empty list

(car (cdr '((b) (x y) ((c)))))
(cdr (cdr '((b) (x y) ((c)))))
;(cdr (car '(a (b (c)) d))) ; cannot ask for the car of an atom

(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '())
(cons 'a '())
(cons '((a b c)) 'b) ; not sure what (((a b c)) . b) means. book suggest that should be no answer
(cons 'a 'b) ; ditto
(cons 'a (car '((b) c d)))
(cons 'a (cdr '((b) c d)))

;;null?
(null? (quote())) ;#t
(null? '(a b c)) ; #f
(null? 'spaghetti)

;;atom?
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(atom? 'Harry)
(atom? '(Harry had a heap of apples))
(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry had a heap of apples)))
(atom? (car (cdr '(swing low sweet cherry oat)))) ;#t
(atom? (car (cdr '(swing (low sweet) cherry pat)))) ;#f

;;eq?
(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(eq? '() '(strawberry))
(eq? 6 7)
(eq? 6 6)
(eq? (car '(Mary had a little lamb chop)) 'Mary)
(eq? (cdr '(soured milk)) 'milk)
(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans))))
