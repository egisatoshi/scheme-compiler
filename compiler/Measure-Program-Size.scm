;;;
;;; Measure-Program-Size (Auxiliary Function)
;;;

(define-module Measure-Program-Size
  (use srfi-1)
  (use util.match)
  (require "./Basic-Utility")
  (import Basic-Utility)
  (export Measure-Program-Size
          ))
(select-module Measure-Program-Size)

(define measure-program-size
  (lambda (exp)
    (match exp
      (() 0)
      ((? boolean? bool) 0)
      ((? integer? int) 0)
      ((? float? float) 0)
      ((? var? var) 0)
      (('Cons A W c)
       (+ (length A) 2 (measure-program-size c)))
      (('Vector A W c)
       (+ (length A) 2 (measure-program-size c)))
      (('Select i r W c)
       (+ 1 (measure-program-size c)))
      (('Alloc (? imd? i) v W c)
       (+ i (measure-program-size c)))
      (('Alloc i v W c) ;; arbitary
       (+ 10 (measure-program-size c)))
      (('Put i r v () c)
       (+ 1 (measure-program-size c)))
      (('Offset i r W c)
       (+ 1 (measure-program-size c)))
      (('Primop i r W c)
       (+ 1 (measure-program-size c)))
      (('If t tc fc)
       (+ 1 (measure-program-size tc) (measure-program-size fc)))
      (('Fix B A)
       (+ (fold (lambda (F n)
                  (match F
                    ((_ _ C) (+ 1 (measure-program-size C)))))
                0
                B)
          (measure-program-size A)))
      (('Fix2 B A)
       (+ (fold (lambda (F n)
                  (match F
                    ((_ _ C) (+ 1 (measure-program-size C)))))
                0
                B)
          (measure-program-size A)))
      (('Apply f A)
       (+ 1 (length A)))
      (('Set v W c)
       (+ 1 (measure-program-size c)))
      (else (errorf "~s : no match expressin : ~s\n" "measure-program-size" exp)))))

(define Measure-Program-Size measure-program-size)

(provide "Measure-Program-Size")