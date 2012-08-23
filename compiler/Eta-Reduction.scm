;;;
;;; Eta Reduction
;;;

(define-module Eta-Reduction
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Empty-Fix-Elimination")
  (require "./Propagation")
  (require "./Substitution")
  (import Basic-Utility)
  (import Assoc)
  (import Empty-Fix-Elimination)
  (import Propagation)
  (import Substitution)
  (export eta-reduct-count
          Eta-Reduct
          ))
(select-module Eta-Reduction)

(define eta-reduct-count 0)

(define can-eta-reduct?
  (lambda (bounder)
    (match bounder
      ((_ v ('Apply _ args)) (equal? v args))
      (else #f))))

(define eta-reduct
  (lambda (exp)
    (match exp
      (('Fix B A)
       (let-values (((tfs ffs) (partition can-eta-reduct? B)))
         (let* ((assoc (map (match-lambda
                             ((f _ ('Apply g _))
                              `(,f ,g)))
                            tfs))
                (new-B (map (match-lambda
                             ((f V B)
                              (let ((new-B (eta-reduct (Substitute B (map car assoc) (map cadr assoc)))))
                                `(,f ,V ,new-B))))
                            ffs))
                (new-A (eta-reduct (Substitute A (map car assoc) (map cadr assoc)))))
           (set! eta-reduct-count (+ eta-reduct-count (length tfs)))
           `(Fix ,new-B ,new-A))))
      (('Fix2 B A) ;; do same thing with Fix
       (let-values (((tfs ffs) (partition can-eta-reduct? B)))
         (let* ((assoc (map (match-lambda
                             ((f _ ('Apply g _))
                              `(,f ,g)))
                            tfs))
                (new-B (map (match-lambda
                             ((f V B)
                              (let ((new-B (eta-reduct (Substitute B (map car assoc) (map cadr assoc)))))
                                `(,f ,V ,new-B))))
                            ffs))
                (new-A (eta-reduct (Substitute A (map car assoc) (map cadr assoc)))))
           (set! eta-reduct-count (+ eta-reduct-count (length tfs)))
           `(Fix2 ,new-B ,new-A))))
      (else (Propagate eta-reduct exp)))))

(define Eta-Reduct
  (lambda (program)
    (format (standard-error-port) "Eta-Reduction start...\n")
    (let ((new-program (Eliminate-Empty-Fix (eta-reduct program))))
      (format (standard-error-port) "Eta-Reduction finished.\n")
      (format (standard-error-port) "Eta-Reduct ~s functions.\n" eta-reduct-count)
      new-program)))

(provide "Eta-Reduction")