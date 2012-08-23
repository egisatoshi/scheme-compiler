;;;
;;; Globals (Auxiliary Function)
;;;

(define-module Globals
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Id")
  (require "./Assoc")
  (require "./Propagation")
  (require "./Substitution")
  (import Basic-Utility)
  (import Id)
  (import Assoc)
  (import Propagation)
  (import Substitution)
  (export globals
          global?
          not-global?
          get-new-global-register
          Assign-Global-Register
          ))
(select-module Globals)

(define globals
  '(g0 g1 g2 g3 g4 g5 g6 g7 g8 g9
    g10 g11 g12 g13 g14 g15 g16 g17 g18 g19
    g20 g21 g22 g23 g24 g25 g26 g27 g28 g29
    g30 g31 g32 g33 g34 g35 g36 g37 g38 g39
    g40 g41 g42 g43 g44 g45 g46 g47 g48 g49
    g50 g51 g52 g53 g54 g55 g56 g57 g58 g59
    g60 g61 g62 g63
    ))

(define global?
  (lambda (var)
    (exists? var (append globals '(s0 s1)))))

(define not-global?
  (lambda (var)
    (not (global? var))))

(define get-new-global-register
  (let ((global-regiter-pool
         '(g0 g1 g2 g3 g4 g5 g6 g7 g8 g9
           g10 g11 g12 g13 g14 g15 g16 g17 g18 g19
           g20 g21 g22 g23 g24 g25 g26 g27 g28 g29
           g30 g31 g32 g33 g34 g35 g36 g37 g38 g39
           g40 g41 g42 g43 g44 g45 g46 g47 g48 g49
           g50 g51 g52 g53 g54 g55 g56 g57 g58 g59
           g60 g61 g62 g63
           )))
    (lambda ()
      (let ((ret (car global-regiter-pool)))
        (set! global-regiter-pool (cdr global-regiter-pool))
        ret))))

(define assign-global-register
  (lambda (exp)
    (match exp
      (('Alloc i v0 (_) ('Global-Set _ (w1) c))
       (let ((g (get-new-global-register)))
         `(Alloc ,i ,v0 (,g) ,(assign-global-register (Substitute c `(,w1) `(,g))))))
      (('Vector A (_) ('Global-Set _ (w1) c))
       (let ((g (get-new-global-register)))
         `(Vector ,A (,g) ,(assign-global-register (Substitute c `(,w1) `(,g))))))
      (('Set v0 (_) ('Global-Set _ (w1) c))
       (let ((g (get-new-global-register)))
         `(Set ,v0 (,g) ,(assign-global-register (Substitute c `(,w1) `(,g))))))
      (else (Propagate assign-global-register exp)))))

(define Assign-Global-Register
  (lambda (program)
    (assign-global-register program)))

(provide "Globals")