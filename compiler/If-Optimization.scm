;;;
;;; If Optimization
;;;

(define-module If-Optimization
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./CPS-Language")
  (require "./Propagation")
  (import Basic-Utility)
  (import CPS-Language)
  (import Propagation)
  (export if-optimize-count
          If-Optimize
          ))
(select-module If-Optimization)

(define if-optimize-count 0)

(define if-optimize
  (lambda (exp)
    (match exp
      (('If (? imd? t) tc fc)
       (inc! if-optimize-count)
       (if t
           (if-optimize tc)
           (if-optimize fc)))
      (('If t tc fc)
       (let ((new-tc (if-optimize tc))
             (new-fc (if-optimize fc)))
         `(If ,t ,new-tc ,new-fc)))
      (else (Propagate if-optimize exp)))))

(define If-Optimize
  (lambda (program)
    (if-optimize program)))

(provide "If-Optimization")