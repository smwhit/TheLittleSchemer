;Commandment #1
;when recurring on a list of atoms, ask null? l and else
;when recurring on a number, ask zero? and else
;when recurring in a list of S-expressions l, ask null?, (atom? (car l)) and else

;Commandment #4
;Always change at least one argument while recurring
;When recurring on a list of atoms, lat, use (cdr lat)
;When recurring on a number use (sub1 n)
;When recurring on a list of S-expressions, l, use (car l) and (cdr l),
;if neither (null? l) nor (atom? (car l)) are true

;Commandment #8
;Use help functions to abstract from representations