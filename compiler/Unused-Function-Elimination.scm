;;;
;;; Unnsed Function Elimination (Auxiliary Function)
;;;

(define-module Unused-Function-Elimination
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Substitution")
  (require "./Empty-Fix-Elimination")
  (require "./Propagation")
  (import Basic-Utility)
  (import Substitution)
  (import Empty-Fix-Elimination)
  (import Propagation)
  (export apply-count
          occur-count
          recursive-function?
          escape-function?
          Eliminate-Unused-Function
          ))
(select-module Unused-Function-Elimination)

(define apply-count
  (lambda (fn exp)
    (match exp
      (() 0)
      ((? boolean? bool) 0)
      ((? integer? int) 0)
      ((? float? float) 0)
      ((? var? var) 0)
      (('Cons _ _ c)
       (apply-count fn c))
      (('Vector _ _ c)
       (apply-count fn c))
      (('Stack _ _ _ c)
       (apply-count fn c))
      (('Select _ _ _ c)
       (apply-count fn c))
      (('Alloc _ _ _ c)
       (apply-count fn c))
      (('Put _ _ _ () c)
       (apply-count fn c))
      (('Offset _ _ _ c)
       (apply-count fn c))
      (('Primop _ _ _ c)
       (apply-count fn c))
      (('If _ tc fc)
       (+ (apply-count fn tc) (apply-count fn fc)))
      (('Fix B A)
       (let ((count-B (map (match-lambda
                            ((f V C) (apply-count fn C)))
                           B))
             (count-A (apply-count fn A)))
         (+ (apply + count-B) count-A)))
      (('Fix2 B A)
       (let ((count-B (map (match-lambda
                            ((f V C) (apply-count fn C)))
                           B))
             (count-A (apply-count fn A)))
         (+ (apply + count-B) count-A)))
      (('Apply f A)
       (if (eq? fn f)
           1
           0))
      (('Set v (w) c)
       (apply-count fn c))
      (('Global-Set v (w) c)
       (apply-count fn c))
      (else (errorf "~s : no match expressin : ~s\n" apply-count exp)))))

(define occur-count
  (lambda (fn exp)
    (match exp
      (() 0)
      ((? boolean? bool) 0)
      ((? integer? int) 0)
      ((? float? float) 0)
      ((? var? var) (if (eq? fn var) 1 0))
      (('Cons A W c)
       (+ (member-count fn A) (occur-count fn c)))
      (('Vector A W c)
       (+ (member-count fn A) (occur-count fn c)))
      (('Stack s A W c)
       (+ (occur-count fn s) (member-count fn A) (occur-count fn c)))
      (('Select i r W c)
       (+ (occur-count fn i) (occur-count fn r) (occur-count fn c)))
      (('Alloc i v W c)
       (+ (occur-count fn i) (occur-count fn v) (occur-count fn c)))
      (('Put i r v () c)
       (+ (occur-count fn i) (occur-count fn r) (occur-count fn v) (occur-count fn c)))
      (('Offset i r W c)
       (+ (occur-count fn i) (occur-count fn r) (occur-count fn c)))
      (('Primop i A W c)
       (+ (occur-count fn i) (member-count fn A) (occur-count fn c)))
      (('If t tc fc)
       (+ (occur-count fn t) (occur-count fn tc) (occur-count fn fc)))
      (('Fix B A)
       (let ((count-B (map (match-lambda
                            ((f V C) (occur-count fn C)))
                           B))
             (count-A (occur-count fn A)))
         (+ (apply + count-B) count-A)))
      (('Fix2 B A)
       (let ((count-B (map (match-lambda
                            ((f V C) (occur-count fn C)))
                           B))
             (count-A (occur-count fn A)))
         (+ (apply + count-B) count-A)))
      (('Apply f A)
       (+ (occur-count fn f) (member-count fn A)))
      (('Set v (w) c)
       (+ (occur-count fn v) (occur-count fn c)))
      (('Global-Set v (w) c)
       (+ (occur-count fn v) (occur-count fn c)))
      (else (errorf "~s : no match expressin : ~s\n" occur-count exp)))))

(define recursive-function?
  (lambda (fn exp) ;; fix lator
    (let ((applyed-all (apply-count fn exp))
          (applyed (match exp
                     (('Fix B A) (apply-count fn A))
                     (('Fix2 B A) (apply-count fn A)))))
      (not (and (= applyed-all applyed))))))

(define escape-function?
  (lambda (fn exp)
    (let ((occured (occur-count fn exp))
          (applyed (apply-count fn exp)))
      (not (and (= occured applyed))))))

(define do-not-used?
  (lambda (F exp)
    (match F
      ((f _ C)
       (= (occur-count f C) (occur-count f exp))))))

(define eliminate-unused-function
  (lambda (exp)
    (match exp
      (('Fix B A)
       (let* ((midd-B (filter (lambda (b) (not (do-not-used? b `(Fix ,B ,A))))
                              B))
              (new-B (map (match-lambda
                           ((f V C) `(,f ,V ,(eliminate-unused-function C))))
                          midd-B))
              (new-A (eliminate-unused-function A)))
         `(Fix ,new-B ,new-A)))
      (('Fix2 B A)
       (let* ((midd-B (filter (lambda (b) (not (do-not-used? b `(Fix2 ,B ,A))))
                              B))
              (new-B (map (match-lambda
                           ((f V C) `(,f ,V ,(eliminate-unused-function C))))
                          midd-B))
              (new-A (eliminate-unused-function A)))
         `(Fix2 ,new-B ,new-A)))
      (else (Propagate eliminate-unused-function exp)))))

(define Eliminate-Unused-Function
  (lambda (program)
    (Eliminate-Empty-Fix (eliminate-unused-function program))))

(provide "Unused-Function-Elimination")