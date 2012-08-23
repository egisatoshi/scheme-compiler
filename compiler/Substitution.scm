;;;
;;; Substitution (Auxiliary Function)
;;;

(define-module Substitution
  (use srfi-1)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Assoc")
  (import Basic-Utility)
  (import Assoc)
  (export Substitute
          ))
(select-module Substitution)

(define map-substitute
  (lambda (ls assoc)
    (map (lambda (l)
           (substitute l assoc))
         ls)))

(define substitute
  (lambda (exp assoc)
    (match exp
      (() ())
      ((? boolean? bool) bool)
      ((? integer? int) int)
      ((? float? float) float)
      ((? var? var)
       (get-value var assoc))
      (('Cons A W c)
       (let ((new-A (map-substitute A assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Cons ,new-A ,new-W ,new-c)))
      (('Vector A W c)
       (let ((new-A (map-substitute A assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Vector ,new-A ,new-W ,new-c)))
      (('Stack s A W c)
       (let ((new-s (substitute s assoc))
             (new-A (map-substitute A assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Stack ,new-s ,new-A ,new-W ,new-c)))
      (('Select i r W c)
       (let ((new-i (substitute i assoc))
             (new-r (substitute r assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Select ,new-i ,new-r ,new-W ,new-c)))
      (('Alloc i v W c)
       (let ((new-i (substitute i assoc))
             (new-v (substitute v assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Alloc ,new-i ,new-v ,new-W ,new-c)))
      (('Put i r v () c)
       (let ((new-i (substitute i assoc))
             (new-r (substitute r assoc))
             (new-v (substitute v assoc))
             (new-c (substitute c assoc)))
         `(Put ,new-i ,new-r ,new-v () ,new-c)))
      (('Offset i r W c)
       (let ((new-i (substitute i assoc))
             (new-r (substitute r assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Offset ,new-i ,new-r ,new-W ,new-c)))
      (('Primop i A W c)
       (let ((new-A (map-substitute A assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Primop ,i ,new-A ,new-W ,new-c)))
      (('If t tc fc)
       (let ((new-t (substitute t assoc))
             (new-tc (substitute tc assoc))
             (new-fc (substitute fc assoc)))
         `(If ,new-t ,new-tc ,new-fc)))
      (('Fix bound-list A)
       (let ((new-bound-list (map (match-lambda
                                   ((f V B)
                                    (let ((new-f (substitute f assoc))
                                          (new-V (map-substitute V assoc))
                                          (new-B (substitute B assoc)))
                                      `(,new-f ,new-V ,new-B))))
                                  bound-list))
             (new-A (substitute A assoc)))
         `(Fix ,new-bound-list ,new-A)))
      (('Fix2 bound-list A)
       (let ((new-bound-list (map (match-lambda
                                   ((f V B)
                                    (let ((new-f (substitute f assoc))
                                          (new-V (map-substitute V assoc))
                                          (new-B (substitute B assoc)))
                                      `(,new-f ,new-V ,new-B))))
                                  bound-list))
             (new-A (substitute A assoc)))
         `(Fix2 ,new-bound-list ,new-A)))
      (('Apply f A)
       (let ((new-f (substitute f assoc))
             (new-A (map-substitute A assoc)))
         `(Apply ,new-f ,new-A)))
      (('Set v W c)
       (let ((new-v (substitute v assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Set ,new-v ,new-W ,new-c)))
      (('Global-Set v W c)
       (let ((new-v (substitute v assoc))
             (new-W (map-substitute W assoc))
             (new-c (substitute c assoc)))
         `(Global-Set ,new-v ,new-W ,new-c)))
      (else (errorf "~s : no match expressin : ~s\n" "Sunstitute" exp)))))

(define Substitute
  (lambda (program X E)
    (substitute program (zip X E))))

(provide "Substitution")