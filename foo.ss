(define hello
  (lambda (s)
    '(hello s)))

(hello 'simon)

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add (add1 n) (sub1 m))))))

(add 10 22)
