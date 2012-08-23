;;;
;;; CPS Conversion
;;;

(define-module CPS-Conversion
  (use srfi-1)
  (use util.match)
  (require "./CPS-Language")
  (require "./Basic-Utility")
  (require "./Id")
  (require "./Globals")
  (require "./Alpha-Transformation")
  (require "./Set-Elimination")
  (import CPS-Language)
  (import Basic-Utility)
  (import Id)
  (import Globals)
  (import Alpha-Transformation)
  (import Set-Elimination)
  (export stack-enable-option
          CPS-Convert
          ))
(select-module CPS-Conversion)

(define stack-enable-option #f)

(define gen-Fix2
  (lambda ()
    (if stack-enable-option
        'Fix2
        'Fix)))

(define cps-convert-list-helper
  (lambda (args cont)
    (letrec ((g (lambda (args w)
                  (if (null? args)
                      (cont (reverse w)) 
                      (let ((e (car args))
                            (r (cdr args)))
                        (cps-convert e
                                     (lambda (v)
                                       (g r (cons v w)))))))))
      (g args '()))))

(define cps-convert
  (lambda (exp cont)
    (match exp
      (() (cont ()))
      ((? boolean? bool) (cont bool))
      ((? integer? int) (cont int))
      ((? float? float) (cont float))
      ((? var? var) (cont var))
      (('cons . A)
       (if (null? A)
           (cont 0)
           (let ((tl (gen-record)))
             (cps-convert-list-helper A
                                      (lambda (new-A)
                                        `(Cons ,new-A (,tl) ,(cont tl)))))))
      (('car A)
       (let ((tw (gentmpv)))
         (cps-convert A
                      (lambda (new-A)
                        `(Select 0 ,new-A (,tw) ,(cont tw))))))
      (('cdr A)
       (let ((tw (gentmpv)))
         (cps-convert A
                      (lambda (new-A)
                        `(Select 1 ,new-A (,tw) ,(cont tw))))))
      (('set-car! A v)
       (cps-convert A
                    (lambda (new-A)
                      (cps-convert v
                                   (lambda (new-v)
                                     `(Put 0 ,new-A ,new-v () ,(cont 0)))))))
      (('set-cdr! A v)
       (cps-convert A
                    (lambda (new-A)
                      (cps-convert v
                                   (lambda (new-v)
                                     `(Put 1 ,new-A ,new-v () ,(cont 0)))))))
      (('vector . A)
       (if (null? A)
           (cont 0)
           (let ((tl (gen-record)))
             (cps-convert-list-helper A
                                      (lambda (new-A)
                                        `(Vector ,new-A (,tl) ,(cont tl)))))))
      (('vector-ref A i)
       (let ((tw (gentmpv)))
         (cps-convert i
                      (lambda (new-i)
                        (cps-convert A
                                     (lambda (new-A)
                                       `(Select ,new-i ,new-A (,tw) ,(cont tw))))))))
      (('make-vector i v)
       (let ((tw (gen-record)))
         (cps-convert i
                      (lambda (new-i)
                        (cps-convert v
                                     (lambda (new-v)
                                       `(Alloc ,new-i ,new-v (,tw) ,(cont tw))))))))
      (('vector-set! A i v)
       (cps-convert i
                    (lambda (new-i)
                      (cps-convert A
                                   (lambda (new-A)
                                     (cps-convert v
                                                  (lambda (new-v)
                                                    `(Put ,new-i ,new-A ,new-v () ,(cont 0)))))))))
      (((? primop? op) . A)
       (if (no-return-primop? op)
           (cps-convert-list-helper A
                                    (lambda (new-A)
                                      `(Primop ,op ,new-A () ,(cont 0))))
           (let ((tl (gentmpv)))
             (cps-convert-list-helper A
                                      (lambda (new-A)
                                        (if (eq? op '/)
                                            `(Primop quotient ,new-A (,tl) ,(cont tl))
                                            `(Primop ,op ,new-A (,tl) ,(cont tl))))))))
      (('if t te fe)
       (let ((tk (gentmpr))
             (tx (gentmpv)))
         `(,(gen-Fix2) ((,tk (,tx) ,(cont tx)))
               ,(cps-convert t
                             (lambda (new-t)
                               `(If ,new-t
                                    ,(cps-convert te
                                                  (lambda (new-te)
                                                    `(Apply ,tk (,new-te))))
                                    ,(cps-convert fe
                                                  (lambda (new-fe)
                                                    `(Apply ,tk (,new-fe))))))))))
      (('lambda A . B)
       (let ((tf (gentmpv))
             (tr (gentmpr)))
         (let ((new-A (cons tr A)))
           `(Fix ((,tf ,new-A ,(cps-convert (cons 'begin B)
                                               (lambda (z)
                                                 `(Apply ,tr (,z))))))
                 ,(cont tf)))))
      (('apply f ('list . A))
       (let ((tr (gentmpr))
             (tx (gentmpv)))
         `(,(gen-Fix2) ((,tr (,tx) ,(cont tx)))
                ,(cps-convert f
                              (lambda (new-f)
                                (cps-convert-list-helper A
                                                         (lambda (new-A)
                                                           `(Apply ,new-f (,tr . ,new-A)))))))))
      (('begin . E)
       (if (null? E)
           (cont 0)
           (letrec ((g (lambda (E)
                         (let ((e (car E))
                               (rest-E (cdr E)))
                           (if (null? rest-E)
                               (cps-convert e (lambda (new-e) (cont new-e)))
                               (cps-convert e (lambda (new-e)
                                                (g rest-E))))))))
             (g E))))
      (('letrec bound-list . B)
       (let ((new-bound-list (map (lambda (bounder)
                                    (match bounder
                                      ((var val)
                                       (match val
                                         ((_ args . B)
                                          (let ((tw (gentmpr)))
                                            `(,var (,tw . ,args)
                                                   ,(cps-convert (cons 'begin B)
                                                                 (lambda (z)
                                                                   `(Apply ,tw (,z)))))))))))
                                  bound-list)))
         `(Fix ,new-bound-list
               ,(cps-convert (cons 'begin B) cont))))
      (('let bound-list . B)
       (letrec ((g (lambda (bound-list B)
                     (if (null? bound-list)
                         (cps-convert (cons 'begin B) cont)
                         ((match-lambda
                           ((v e)
                            (cps-convert e
                                         (lambda (new-e)
                                           `(Set ,new-e (,v) ,(g (cdr bound-list) B))))))
                          (car bound-list))))))
         (g bound-list B)))
      (('let-global bound-list . B)
       (letrec ((g (lambda (bound-list B)
                     (if (null? bound-list)
                         (cps-convert (cons 'begin B) cont)
                         ((match-lambda
                           ((v e)
                            (cps-convert e
                                         (lambda (new-e)
                                           `(Global-Set ,new-e (,v) ,(g (cdr bound-list) B))))))
                          (car bound-list))))))
         (g bound-list B)))
      (('receive F E B)
       (cps-convert E
                    (lambda (new-E)
                      (let ((A (cons 'list new-E)))
                        (cps-convert `(apply (lambda ,F ,B) ,A)
                                     (lambda (r) (cont r)))))))
      (('call-with-values ('lambda () E) c)
       (cps-convert E
                    (lambda (new-E)
                      (let ((A (cons 'list new-E)))
                        (cps-convert `(apply ,c ,A)
                                     (lambda (r) (cont r)))))))
      (('let-values ((F E)) B)
       (cps-convert E
                    (lambda (new-E)
                      (let ((A (cons 'list new-E)))
                        (cps-convert `(apply (lambda ,F ,B) ,A)
                                     (lambda (r) (cont r)))))))
      (('values . A)
       (cps-convert-list-helper A
                                (lambda (new-A)
                                  (cont new-A))))
      (('call/cc f)
       (let ((tk (gentmpv))
             (tx (gentmpv)))
         `(Fix ((,tk (,tx) ,(cont tx)))
               ,(cps-convert f
                             (lambda (new-f)
                               `(Apply ,new-f (,tk ,tk)))))))
      (('throw k (list . A))
       (cps-convert k
                    (lambda (new-k)
                      (cps-convert-list-helper A
                                               (lambda (new-A)
                                                 `(Apply ,new-k ,new-A))))))
      ((f . A) ;; must be last
       (cps-convert `(apply ,f (list . ,A)) cont)))))

(define CPS-Convert
  (lambda (program)
    (format (standard-error-port) "CPS-Conversion start...\n")
    (let ((new-program (Eliminate-Set
                        (Alpha-Transform
                         (Assign-Global-Register
                          (cps-convert program (lambda (x) x)))))))
      (format (standard-error-port) "CPS-Conversion finished.\n")
      new-program)))

(provide "CPS-Conversion")