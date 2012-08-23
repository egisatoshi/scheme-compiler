;;;
;;; Liftup Fixes
;;;

(define-module Liftup-Fixes
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Id")
  (require "./Substitution")
  (require "./Propagation")
  (import Basic-Utility)
  (import Id)
  (import Substitution)
  (import Propagation)
  (export Liftup-Fixes
          ))
(select-module Liftup-Fixes)

(define liftup-Fixes
  (lambda (exp)
    (match exp
      (() (values '() ()))
      ((? boolean? bool) (values '() bool))
      ((? integer? int) (values '() int))
      ((? float? float) (values '() float))
      ((? var? var) (values '() var))
      (('Cons A W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Cons ,A ,W ,new-c))))
      (('Vector A W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Vector ,A ,W ,new-c))))
      (('Stack s A W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Stack ,s ,A ,W ,new-c))))
      (('Select i r W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Select ,i ,r ,W ,new-c))))
      (('Alloc i v W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Alloc ,i ,v ,W ,new-c))))
      (('Put i r v () c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Put ,i ,r ,v () ,new-c))))
      (('Offset i r W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Offset ,i ,r ,W ,new-c))))
      (('Primop i A W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Primop ,i ,A ,W ,new-c))))
      (('If t tc fc)
       (let-values (((F-tc new-tc) (liftup-Fixes tc))
                    ((F-fc new-fc) (liftup-Fixes fc)))
         (values (append F-tc F-fc) `(If ,t ,new-tc ,new-fc))))
      (('Fix B A)
       (let ((F-B (let ((F (map (match-lambda
                                 ((f V C)
                                  (let-values (((F-C new-C) (liftup-Fixes C)))
                                    `(,F-C (,f ,V ,new-C)))))
                                B)))
                    (let-values (((F-Cs F-Bs) (unzip2 F)))
                      (append F-Bs (apply append F-Cs))))))
         (let-values (((F-A new-A) (liftup-Fixes A)))
           (values (append F-B F-A) new-A))))
      (('Fix2 B A)
       (let ((F-B (let ((F (map (match-lambda
                                 ((f V C)
                                  (let-values (((F-C new-C) (liftup-Fixes C)))
                                    `(,F-C (,f ,V ,new-C)))))
                                B)))
                    (let-values (((F-Cs F-Bs) (unzip2 F)))
                      (append F-Bs (apply append F-Cs))))))
         (let-values (((F-A new-A) (liftup-Fixes A)))
           (values (append F-B F-A) new-A))))
      (('Apply f A)
       (values '() `(Apply ,f ,A)))
      (('Set v W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Set ,v ,W ,new-c))))
      (('Global-Set v W c)
       (let-values (((F-c new-c) (liftup-Fixes c)))
         (values F-c `(Global-Set ,v ,W ,new-c))))
      (else (errorf "~s : no match expressin : ~s\n" "liftup-Fixes" exp)))))

(define Liftup-Fixes
  (lambda (program)
    (let-values (((F A) (liftup-Fixes program)))
      (let* ((new-program `(Fix ,F ,A))
             (L (map car F))
             (new-L (gen-label-list (length L))))
        (Substitute new-program L new-L)))))

(provide "Liftup-Fixes")