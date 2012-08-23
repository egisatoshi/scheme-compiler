;;;
;;; Closure Conversion
;;;

(define-module Closure-Conversion
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Id")
  (require "./Set")
  (require "./Assoc")
  (require "./Propagation")
  (require "./Free-Variable")
  (require "./Globals")
  (require "./Unused-Function-Elimination")
  (require "./Alpha-Transformation")
  (import Basic-Utility)
  (import Id)
  (import Set)
  (import Assoc)
  (import Propagation)
  (import Free-Variable)
  (import Globals)
  (import Unused-Function-Elimination)
  (import Alpha-Transformation)
  (export nonclosure-function-count
          closure-function-count
          Closure-Convert
          ))
(select-module Closure-Conversion)

(define nonclosure-function-count 0)
(define closure-function-count 0)

(define nonclosure-functions empty-set)

(define get-apply-fns
  (lambda (exp)
    (match exp
      (() empty-set)
      ((? boolean? bool) empty-set)
      ((? integer? int) empty-set)
      ((? float? float) empty-set)
      ((? var? var) empty-set)
      (('Cons _ _ c) (get-apply-fns c))
      (('Vector _ _ c) (get-apply-fns c))
      (('Stack _ _ _ c) (get-apply-fns c))
      (('Select _ _ _ c) (get-apply-fns c))
      (('Alloc _ _ _ c) (get-apply-fns c))
      (('Put _ _ _ _ c) (get-apply-fns c))
      (('Offset _ _ _ c) (get-apply-fns c))
      (('Primop _ _ _ c) (get-apply-fns c))
      (('If _ tc fc) (set-union (get-apply-fns tc) (get-apply-fns fc)))
      (('Fix B A)
       (let ((fn-B (fold set-union empty-set
                         (map (match-lambda
                               ((_ _ C) (get-apply-fns C)))
                              B)))
             (fn-A (get-apply-fns A)))
         (set-union fn-B fn-A)))
      (('Fix2 B A)
       (let ((fn-B (fold set-union empty-set
                         (map (match-lambda
                               ((_ _ C) (get-apply-fns C)))
                              B)))
             (fn-A (get-apply-fns A)))
         (set-union fn-B fn-A)))
      (('Apply f _) (singleton f))
      (('Set _ _ c) (get-apply-fns c))
      (else (errorf "~s : no match expressin : ~s\n" get-apply-fns exp)))))

(define calc-closure-fns
  (lambda (B escape-fns)
    (let ((ret-fns (map car (filter (match-lambda
                                     ((f _ C)
                                      (if (exists? f escape-fns)
                                          #t
                                          #|
                                          (let ((apply-fns (get-apply-fns C)))
                                            (not (null? (set-intersection escape-fns
                                                                          apply-fns)))))))
                                          |#
                                          (let ((fv-f (Free-Variable C)))
                                            (not (null? (set-intersection escape-fns
                                                                          fv-f)))))))
                                    B))))
      (if (equal? ret-fns escape-fns)
          ret-fns
          (begin
;            (print ret-fns escape-fns)
            (calc-closure-fns B ret-fns))))))

(define partision-nonclosure-function
  (lambda (B exp)
    (let* ((fns (map car B))
           (fn-b (zip fns B))
           (escape-fns (filter (lambda (f) (escape-function? f exp)) fns))
           (closure-fns (calc-closure-fns B escape-fns))
           (nonclosure-fns (set-diff fns closure-fns)))
      (values (map (lambda (fn) (get-value fn fn-b)) nonclosure-fns)
              (map (lambda (fn) (get-value fn fn-b)) closure-fns)))))
;      (values '() B))))

(define calc-fv-noncls
  (lambda (B)
    (let* ((fs (map car B))
           (appf-f (map (match-lambda
                         ((f V C)
                          `(,f ,(set-intersection (get-apply-fns C) fs))))
                        B))
           (init-f-fv (map (match-lambda
                            ((f V C)
                             `(,f ,(set-diff (Free-Variable C) fs V nonclosure-functions globals))))
                           B)))
      (let loop ((f-fv init-f-fv))
        (let ((new-f-fv (map (match-lambda
                              ((f fv-f)
                               (let ((apply-fns (get-value f appf-f)))
                                 `(,f ,(set-union fv-f
                                                  (apply set-union
                                                         (map (lambda (fn)
                                                                (get-value fn f-fv))
                                                              apply-fns)))))))
                             f-fv)))
          (if (equal? new-f-fv f-fv)
              f-fv
              (loop new-f-fv)))))))

(define calc-fm
  (lambda (B)
    (let ((fs (map car B)))
      (map (match-lambda
            ((f V C)
             `(,f ,(set-delete (set-intersection (Free-Variable C) fs) f))))
           B))))

(define convert-Apply-nonCls
  (lambda (exp fs fv-B)
    (match exp
      (('Apply f A)
       (if (exists? f fs)
           `(Apply ,f ,(append A (get-value f fv-B)))
           `(Apply ,f ,A)))
      (else (Propagate convert-Apply-nonCls exp fs fv-B)))))

(define convert-Fix-nonClsFns
  (lambda (exp)
    (match exp
      (('Fix B A)
       (let* ((fs (map car B))
              (fv-B (calc-fv-noncls B))
              (midd-B (map (match-lambda
                            ((f V C)
                             (let ((fv-f (get-value f fv-B)))
                               `(,f ,(append V fv-f) ,C))))
                           B)))
         (convert-Apply-nonCls `(Fix ,midd-B ,A) fs fv-B))))))

(define calc-fv-cls
  (lambda (B)
    (let ((fs (map car B)))
      (map (match-lambda
            ((f V C)
             `(,f ,(set-diff (Free-Variable C) fs V nonclosure-functions globals))))
           B))))

(define insert-Offsets
  (lambda (exp Cls f fms f-ft index)
    (if (null? fms)
        exp
        (let ((fm (car fms)))
          `(Offset ,(- (findn (get-value fm f-ft) Cls) index) ,f (,fm)
                   ,(insert-Offsets exp Cls f (cdr fms) f-ft index))))))

(define insert-Selects
  (lambda (exp Cls f fv-f index)
    (if (null? fv-f)
        exp
        (let ((v (car fv-f)))
          `(Select ,(- (findn v Cls) index) ,f (,v)
                   ,(insert-Selects exp Cls f (cdr fv-f) index))))))

(define insert-Vectors
  (lambda (exp Cls fs)
    (letrec ((insert-Vectors-iter
              (lambda (exp f fs index)
                (if (null? fs)
                    exp
                    `(Offset ,index ,f (,(car fs))
                             ,(insert-Vectors-iter exp f (cdr fs) (+ index 1)))))))
      (if (null? fs)
          exp
          `(Vector ,Cls
                   (,(car fs))
                   ,(insert-Vectors-iter exp (car fs) (cdr fs) 1))))))

(define convert-Fix-ClsFns
  (lambda (exp)
    (match exp
      (('Fix B A)
       (let* ((fs (map car B))
              (fts (gentmpv-list (length fs)))
              (f-ft (zip fs fts))
              (f-fms (calc-fm B))
              (fv-B (calc-fv-cls B))
              (Cls (append fts (apply set-union (map cadr fv-B))))
              (new-B (map (match-lambda
                           ((f V C)
                            (let* ((ft (get-value f f-ft))
                                   (fms (get-value f f-fms))
                                   (new-V (cons f V))
                                   (fv-f (get-value f fv-B))
                                   (new-C (insert-Offsets (insert-Selects C Cls f fv-f (findn ft Cls))
                                                          Cls f fms f-ft (findn ft Cls))))
                              `(,ft ,new-V ,new-C))))
                          B))
              (new-A (insert-Vectors A Cls fs)))
         `(Fix ,new-B ,new-A))))))

(define convert-Fix
  (lambda (exp)
    (match exp
      (('Fix B A)
       (let-values (((nonClsFns ClsFns) (partision-nonclosure-function B exp)))
         (set! nonclosure-function-count (+ nonclosure-function-count (length nonClsFns)))
         (set! closure-function-count (+ closure-function-count (length ClsFns)))
         (set! nonclosure-functions (set-union nonclosure-functions (map car nonClsFns)))
         (let* ((midd-exp `(Fix ,nonClsFns (Fix ,ClsFns ,A)))
                (midd-exp2 (convert-Fix-nonClsFns midd-exp)))
           (match midd-exp2
             (('Fix C A2)
              (let ((new-A2 (convert-Fix-ClsFns A2)))
                `(Fix ,C ,new-A2)))))))
      (else (Propagate convert-Fix exp)))))

(define insert-Stack
  (lambda (exp Cls f stack-top)
    `(Stack ,stack-top ,Cls (,f) ,exp)))

(define convert-Fix2
  (lambda (exp stack-top)
    (match exp
      (('Fix2 ((f V C)) A)
       (let* ((ft (gentmpv))
              (fv-B (calc-fv-cls `((,f ,V ,C))))
              (Cls (append `(,ft) (apply set-union (map cadr fv-B))))
              (new-B (map (match-lambda
                           ((f V C)
                            (let* ((new-V (cons f V))
                                   (fv-f (get-value f fv-B))
                                   (new-C (insert-Selects C Cls f fv-f (findn ft Cls))))
                              `(,ft ,new-V ,new-C))))
                          `((,f ,V ,C))))
              (new-A (insert-Stack A Cls f stack-top)))
         (set! closure-function-count (+ closure-function-count 1))
         `(Fix2 ,new-B ,new-A))))))

(define closure-convert
  (lambda (exp stack-top)
    (match exp
      (('Fix B A)
       (let ((new-exp (convert-Fix `(Fix ,B ,A))))
         (match new-exp
           (('Fix midd-B1 ('Fix midd-B2 C))
            (let ((new-B1 (map (match-lambda ;; unclosure functions
                                ((f V D) `(,f ,V ,(closure-convert D (car V)))))
                               midd-B1))
                  (new-B2 (map (match-lambda ;; closure functions
                                ((f V D) `(,f ,V ,(closure-convert D (cadr V)))))
                               midd-B2))
                  (new-C (closure-convert C stack-top)))
              `(Fix ,new-B1 (Fix ,new-B2 ,new-C)))))))
      (('Fix2 B A)
       (let ((new-exp (convert-Fix2 `(Fix2 ,B ,A) stack-top)))
         (match new-exp
           (('Fix2 ((f V D)) midd-A)
            (let ((new-B `((,f ,V ,(closure-convert D (car V)))))
                  (new-A (closure-convert midd-A (car V))))
              `(Fix2 ,new-B ,new-A))))))
      (else (Propagate closure-convert exp stack-top)))))

(define convert-Apply
  (lambda (exp)
    (match exp
      (('Apply f A)
       (if (exists? f nonclosure-functions)
           `(Apply ,f ,A)
           (let ((tw (gentmpv)))
             `(Select 0 ,f (,tw) (Apply ,tw ,(cons f A))))))
      (else (Propagate convert-Apply exp)))))

(define Closure-Convert
  (lambda (program)
    (format (standard-error-port) "Start Closure-Conversion...\n")
    (let ((new-program (convert-Apply
                        (closure-convert
                         (Eliminate-Unused-Function
                          program)
                         's1))))
      (format (standard-error-port) "Closure-Conversion Finished.\n")
      (format (standard-error-port) "nonClosure Function : ~s\n" nonclosure-function-count)
      (format (standard-error-port) "Closure Function : ~s\n" closure-function-count)
      new-program)))

(provide "Closure-Conversion")