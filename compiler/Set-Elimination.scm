;;;
;;; Eliminate Set (Auxiliary Function)
;;;

(define-module Set-Elimination
  (use srfi-1)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Substitution")
  (require "./Propagation")
  (import Basic-Utility)
  (import Substitution)
  (import Propagation)
  (export Eliminate-Set
          ))
(select-module Set-Elimination)

(define eliminate-set
  (lambda (exp)
    (match exp
      (('Set v (w) c)
       (let ((new-c (Substitute c `(,w) `(,v))))
         (eliminate-set new-c)))
      (else (Propagate eliminate-set exp)))))

(define Eliminate-Set
  (lambda (program)
    (eliminate-set program)))

(provide "Set-Elimination")