;;;
;;; Primop Optimization
;;;

(define-module Primop-Optimization
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./CPS-Language")
  (require "./Set-Elimination")
  (require "./Propagation")
  (import Basic-Utility)
  (import CPS-Language)
  (import Set-Elimination)
  (import Propagation)
  (export primop-optimize-count
          Primop-Optimize
          ))
(select-module Primop-Optimization)

(define primop-optimize-count 0)

(define primop-optimize
  (lambda (exp)
    (match exp
      (('Primop i () W c) ;; Subeffect
       (let ((new-c (primop-optimize c)))
         `(Primop ,i () ,W ,new-c)))
      (('Primop i A () c) ;; Subeffect
       (let ((new-c (primop-optimize c)))
         `(Primop ,i ,A () ,new-c)))
      (('Primop i ((? imd? a)) W c)
       (inc! primop-optimize-count)
       (let ((v ((eval i interaction-environment) a))
             (new-c (primop-optimize c)))
         `(Set ,v ,W ,new-c)))
      (('Primop i ((? imd? a) (? imd? b)) W c)
       (inc! primop-optimize-count)
       (let ((v ((eval i interaction-environment) a b))
             (new-c (primop-optimize c)))
         `(Set ,v ,W ,new-c)))
      (('Primop i A W c)
       (let ((new-c (primop-optimize c)))
         `(Primop ,i ,A ,W ,new-c)))
      (else (Propagate primop-optimize exp)))))

(define Primop-Optimize
  (lambda (program)
    (Eliminate-Set
     (primop-optimize program))))

(provide "Primop-Optimization")