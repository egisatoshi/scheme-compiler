(define-module CPS-Language
  (use srfi-1)
  (use util.match)
  (export Cons
          Vector
          Stack
          Select
          Alloc
          Put
          Offset
          Primop
          If
          Fix
          Fix2
          Apply
          Set
          record
          make-record
          select
          offset
          put!
          primop?
          no-return-primop?
          commutative-primop?
          +.
          -.
          *.
          /.
          =.
          >.
          <.
          >=.
          <=.
          xor
          logor
          srl
          sll
          ftoi
          itof
          ))
(select-module CPS-Language)

;;
;; CPS language
;;

(define-syntax Cons
  (syntax-rules ()
    ((_ A (w) c)
     (let ((w `(,(list->vector (list . A)) 0))) ((lambda (w) c) w)))))

(define-syntax Vector
  (syntax-rules ()
    ((_ A (w) c)
     (let ((w `(,(list->vector (list . A)) 0))) ((lambda (w) c) w)))))

(define-syntax Stack
  (syntax-rules ()
    ((_ _ A (w) c)
     (let ((w `(,(list->vector (list . A)) 0))) ((lambda (w) c) w)))))

(define-syntax Select
  (syntax-rules ()
    ((_ i r (w) c)
     (let ((V (car r))
           (offset (cadr r)))
       (let ((w (vector-ref V (+ offset i)))) ((lambda (w) c) w))))))

(define-syntax Alloc
  (syntax-rules ()
    ((_ i v (w) c)
     (let ((w `(,(make-vector i v) 0))) ((lambda (w) c) w)))))

(define-syntax Put
  (syntax-rules ()
    ((_ i r v () c)
     (let ((V (car r))
           (offset (cadr r)))
       (vector-set! V (+ offset i) v)
       c))))

(define-syntax Offset
  (syntax-rules ()
    ((_ i r (w) c)
     (let ((V (car r))
           (offset (cadr r)))
       (let ((w `(,V ,(+ offset i))))
         ((lambda (w) c) w))))))

(define-syntax Primop
  (syntax-rules ()
    ((_ i A () c)
     (begin (i . A)
            c))
    ((_ i A (w) c)
     (let ((w (i . A)))
       ((lambda (w) c) w)))))

(define-syntax If
  (syntax-rules ()
    ((_ t te fe)
     (if t te fe))))

(define-syntax Fix
  (syntax-rules ()
    ((_ ((f v B) ...) A)
     (letrec ((f (lambda v B)) ...)
       A))))

(define-syntax Fix2
  (syntax-rules ()
    ((_ ((f v B) ...) A)
     (letrec ((f (lambda v B)) ...)
       A))))

(define-syntax Apply
  (syntax-rules ()
    ((_ f A)
     (apply f (list . A)))))

(define-syntax Set
  (syntax-rules ()
    ((_ t (w) c)
     (let ((w t))
       c))))

(define record
  (lambda A
    `(,(vector A) 0)))

(define make-record
  (lambda (i v)
    `(,(make-vector i v) 0)))

(define select
  (lambda (i A)
    (match A
      ((V offset) (vector-ref V (+ offset i))))))

(define offset
  (lambda (i A)
    (match A
      ((V offset) `(,V ,(+ offset i))))))

(define put!
  (lambda (i A v)
    (match A
      ((V offset) (vector-set! V (+ offset i) v)))))

(define primop-list
  '(+
    -
    *
;    /
;    quotient
;    modulo
    =
    >
    <
    >=
    <=
    +.
    -.
    *.
    /.
    =.
    >.
    <.
    >=.
    <=.
    itof
    ftoi
    and
    or
    xor
    not
    logand
    logor
    logxor
    lognot
    srl
    sll
    read-byte
;    read-word
    write-byte
    write-word
    eq?
    null?
    boolean?
    pair?
    symbol?
    integer?
    real?
    char?
    string?
    vector?
    port?
    procedure?
    ))

(define primop?
  (lambda (op)
    (not (eq? #f (member op primop-list)))))

(define no-return-primop-list
  '(write-byte
    write-word
    ))

(define commutative-primop-list
  '(+
    =
    +.
    =.
    and
    or
    xor
    not
    logand
    logor
    logxor
    ))

(define no-return-primop?
  (lambda (op)
    (not (eq? #f (member op no-return-primop-list)))))

(define commutative-primop?
  (lambda (op)
    (not (eq? #f (member op commutative-primop-list)))))

(define +. +)
(define -. -)
(define *. *)
(define /. /)
(define =. =)
(define >. >)
(define <. <)
(define >=. >=)
(define <=. <=)

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

(provide "CPS-Language")