(define-module Object-Language
  (use srfi-1)
  (use util.match)
  (export throw
          xor
          logor
          srl
          sll
          ftoi
          itof
          =.
          >.
          <.
          >=.
          <=.
          ))
(select-module Object-Language)

(define throw
  (lambda (cont args)
    (apply cont args)))

(define xor
  (lambda (x y)
    (if x
        (not y)
        y)))

(define logor
  (lambda (x y)
    (lognot (logand (lognot x) (lognot y)))))

(define srl
  (lambda (n c)
    (ash n (- c))))

(define sll
  (lambda (n c)
    (ash n c)))

(define ftoi
  (lambda (f)
    (if (>= f 0.0)
        (truncate->exact (+ f 0.5))
        (truncate->exact (- f 0.5)))))

(define itof
  (lambda (i)
    (exact->inexact i)))

(define =. =)
(define >. >)
(define <. <)
(define >=. >=)
(define <=. <=)

(provide "Object-Language")