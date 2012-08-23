;;;
;;; Beta Expansion
;;;

(define-module Beta-Expansion
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Substitution")
  (require "./Empty-Fix-Elimination")
  (require "./Unused-Function-Elimination")
  (require "./Alpha-Transformation")
  (require "./Measure-Program-Size")
  (require "./Propagation")
  (import Basic-Utility)
  (import Substitution)
  (import Unused-Function-Elimination)
  (import Empty-Fix-Elimination)
  (import Alpha-Transformation)
  (import Measure-Program-Size)
  (import Propagation)
  (export beta-expand-count
          Beta-Expand
          ))
(select-module Beta-Expansion)

(define beta-expand-count 0)

(define imd?
  (lambda (x)
    (or (not (symbol? x))
        (let ((s (x->string x)))
          (char=? (string-ref s 0) #\R)))))

(define appear-Fix?
  (lambda (exp)
    (match exp
      (() #f)
      ((? boolean? bool) #f)
      ((? integer? int) #f)
      ((? float? float) #f)
      ((? var? var) #f)
      (('Cons _ _ _) (appear-Fix? c))
      (('Vector _ _ c) (appear-Fix? c))
      (('Stack _ _ _ c) (appear-Fix? c))
      (('Select _ _ _ c) (appear-Fix? c))
      (('Alloc _ _ _ c) (appear-Fix? c))
      (('Put _ _ _ _ c) (appear-Fix? c))
      (('Offset _ _ _ c) (appear-Fix? c))
      (('Primop _ _ _ c) (appear-Fix? c))
      (('If _ tc fc) (or (appear-Fix? tc) (appear-Fix? fc)))
      (('Fix _ _) #t)
      (('Fix2 _ _) #t)
      (('Apply _ _) #f)
      (('Set _ _ c) (appear-Fix? c))
      (else (errorf "~s : no match expressin : ~s\n" appear-Fix? exp)))))

(define can-beta-expand?
  (lambda (F)
    (match F
      ((F V C)
       (and (< (Measure-Program-Size C) 150)
            (not (appear-Fix? C)))))))

(define do-beta-expand?
  (lambda (F A size rec-flag cont-flag)
    (let-values (((I V) (partition imd? A)))
      (or (< size 30)
          (if cont-flag
              (if rec-flag
                  (<= (length V) 0)
                  (> (length I) 0))
              (if rec-flag
                  (<= (length V) 1)
                  (> (length I) 0)))))))

(define do-beta-expand
  (lambda (exp F size rec-flag cont-flag)
    (match exp
      (('Apply f A)
       (match F
         ((fn V B)
          (if (and (eq? fn f)
                   (do-beta-expand? F A size rec-flag cont-flag))
              (begin
                (inc! beta-expand-count)
                (Alpha-Transform (Substitute B V A)))
              `(Apply ,f ,A)))))
      (else (Propagate do-beta-expand exp F size rec-flag cont-flag)))))

(define Do-Beta-Expand
  (lambda (exp)
    (match exp
      (('Fix B A)
       (let loop ((B1 B)
                  (exp exp))
         (if (null? B1)
             exp
             (let ((b (car B1)))
               (match b
                 ((f V C)
                  (if (can-beta-expand? b)
                      (let ((size (Measure-Program-Size C)))
                        (if (recursive-function? f exp)
                            (loop (cdr B1) `(Fix ,B ,(do-beta-expand A b size #t #t)))
                            (loop (cdr B1) `(Fix ,B ,(do-beta-expand A b size #f #t)))))
                      (loop (cdr B1) exp))))))))
      (('Fix2 B A) ;; do same thing with Fix
       (let loop ((B1 B)
                  (exp exp))
         (if (null? B1)
             exp
             (let ((b (car B1)))
               (match b
                 ((f V C)
                  (if (can-beta-expand? b)
                      (let ((size (Measure-Program-Size C)))
                        (if (recursive-function? f exp)
                            (loop (cdr B1) `(Fix2 ,B ,(do-beta-expand A b size #t #t)))
                            (loop (cdr B1) `(Fix2 ,B ,(do-beta-expand A b size #f #t)))))
                      (loop (cdr B1) exp))))))))
      )))

(define beta-expand
  (lambda (exp)
    (match exp
      (('Fix B A)
       (let* ((B1 (map (match-lambda
                          ((f V C) `(,f ,V ,(Beta-Expand C))))
                       B))
              (new-exp (Do-Beta-Expand `(Fix ,B1 ,A))))
         (match new-exp
           (('Fix new-B midd-A)
            (let ((new-A (beta-expand midd-A)))
              `(Fix ,new-B ,new-A))))))
      (('Fix2 B A) ;; do same thing with Fix
       (let* ((B1 (map (match-lambda
                          ((f V C) `(,f ,V ,(Beta-Expand C))))
                       B))
              (new-exp (Do-Beta-Expand `(Fix2 ,B1 ,A))))
         (match new-exp
           (('Fix2 new-B midd-A)
            (let ((new-A (beta-expand midd-A)))
              `(Fix2 ,new-B ,new-A))))))
      (else (Propagate beta-expand exp)))))

(define Beta-Expand
  (lambda (program)
    (Eliminate-Empty-Fix
     (Eliminate-Unused-Function
      (beta-expand
       program)))))

(provide "Beta-Expansion")
