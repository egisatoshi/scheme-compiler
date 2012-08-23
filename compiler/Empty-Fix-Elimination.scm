;;;
;;; Eliminate Empty Fix (Auxiliary Function)
;;;

(define-module Empty-Fix-Elimination
  (use srfi-1)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Propagation")
  (import Basic-Utility)
  (import Propagation)
  (export Eliminate-Empty-Fix
          ))
(select-module Empty-Fix-Elimination)

(define eliminate-empty-fix
  (lambda (exp)
    (match exp
      (('Fix () A)
       (eliminate-empty-fix A))
      (('Fix2 () A)
       (eliminate-empty-fix A))
      (else (Propagate eliminate-empty-fix exp)))))

(define Eliminate-Empty-Fix
  (lambda (program)
    (eliminate-empty-fix program)))

(provide "Empty-Fix-Elimination")