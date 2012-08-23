;;;
;;; Beta Contraction
;;;

(define-module Beta-Contraction
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Substitution")
  (require "./Empty-Fix-Elimination")
  (require "./Unused-Function-Elimination")
  (require "./Propagation")
  (import Basic-Utility)
  (import Substitution)
  (import Unused-Function-Elimination)
  (import Empty-Fix-Elimination)
  (import Propagation)
  (export beta-contract-count
          Beta-Contract
          ))
(select-module Beta-Contraction)

(define beta-contract-count 0)

(define can-beta-contract?
  (lambda (fn exp)
    (let ((occured (occur-count fn exp))
          (applyed (match exp
                     (('Fix B A) (apply-count fn A))
                     (('Fix2 B A) (apply-count fn A)))))
      (and (= occured 1)
           (= applyed 1)))))

(define do-beta-contract
  (lambda (exp F)
    (match exp
      (('Apply f A)
       (match F
         ((fn V B)
          (if (eq? fn f)
              (Substitute B V A)
              `(Apply ,f ,A)))))
      (else (Propagate do-beta-contract exp F)))))

(define Do-Beta-Contract
  (lambda (F exp)
    (inc! beta-contract-count)
    (do-beta-contract exp F)))

(define beta-contract
  (lambda (exp)
    (match exp
      (('Fix B A)
       (let-values (((F new-B)
                     (partition (lambda (b) (can-beta-contract? (car b) `(Fix ,B ,A)))
                                B)))
         (let ((midd-A (fold Do-Beta-Contract A F)))
           (let ((new-A (beta-contract midd-A)))
             `(Fix ,new-B ,new-A)))))
      (('Fix2 B A)
       (let-values (((F new-B)
                     (partition (lambda (b) (can-beta-contract? (car b) `(Fix2 ,B ,A)))
                                B)))
         (let ((midd-A (fold Do-Beta-Contract A F)))
           (let ((new-A (beta-contract midd-A)))
             `(Fix2 ,new-B ,new-A)))))
      (else (Propagate beta-contract exp)))))

(define Beta-Contract
  (lambda (program)
;    (format (standard-error-port) "Beta-Contraction start...\n")
    (let ((new-program (Eliminate-Empty-Fix
                        (Eliminate-Unused-Function
                         (beta-contract
                          (Eliminate-Unused-Function
                           program))))))
;      (format (standard-error-port) "Beta-Contraction finished.\n")
;      (format (standard-error-port) "Beta-Contract ~s functions.\n" beta-contract-count)
      new-program)))

(provide "Beta-Contraction")
