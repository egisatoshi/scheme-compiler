;;;
;;; Primop Optimization
;;;

(define-module Select-Optimization
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./CPS-Language")
  (require "./Propagation")
  (require "./Globals")
  (require "./Set-Elimination")
  (import Basic-Utility)
  (import CPS-Language)
  (import Propagation)
  (import Globals)
  (import Set-Elimination)
  (export select-optimize-count
          Select-Optimize
          ))
(select-module Select-Optimization)

(define select-optimize-count 0)

(define put
  (lambda (i A v)
    (if (= i 0)
        (cons v (cdr A))
        (cons (car A) (put (- i 1) (cdr A) v)))))

(define can-put-optimize?
  (lambda (exp a)
    (match exp
      (() #f)
      ((? boolean? bool) #f)
      ((? integer? int) #f)
      ((? float? float) #f)
      ((? var? var) #f)
      (('Cons _ _ _) (can-put-optimize? c a))
      (('Vector _ _ c) (can-put-optimize? c a))
      (('Stack _ _ _ c) (can-put-optimize? c a))
      (('Select _ r _ c)
       (if (eq? a r)
           #f
           (can-put-optimize? c a)))
      (('Alloc _ _ _ c) (can-put-optimize? c a))
      (('Put _ r _ _ c)
       (if (eq? a r)
           #t
           (can-put-optimize? c a)))
      (('Offset _ _ _ c) (can-put-optimize? c a))
      (('Primop _ _ _ c) (can-put-optimize? c a))
      (('If _ tc fc) #f)
      (('Fix _ A) (can-put-optimize? A a))
      (('Fix2 _ A) (can-put-optimize? A a))
      (('Apply _ _) #f)
      (('Set _ _ c) (can-put-optimize? c a))
      (else (errorf "~s : no match expressin : ~s\n" can-put-optimize? exp)))))

(define do-put-optimize
  (lambda (exp a A Op)
    (match exp
      (('Put (? imd? i) (? (lambda (x) (eq? x a)) r) v () c)
       (inc! select-optimize-count)
       (let ((new-A (put i A v)))
         `(,Op ,new-A (,a) ,c)))
      (('If t tc fc) `(If ,t ,tc ,fc))
      (('Fix B C)
       (let ((new-C (do-put-optimize C a A Op)))
         `(Fix ,B ,new-C)))
      (('Fix2 B C)
       (let ((new-C (do-put-optimize C a A Op)))
         `(Fix2 ,B ,new-C)))
      (else (Propagate do-put-optimize exp a A Op)))))

(define Do-Put-Optimize
  (lambda (exp)
    (match exp
      (('Cons A (w) c)
       (do-put-optimize c w A 'Cons))
      (('Vector A (w) c)
       (do-put-optimize c w A 'Vector)))))

(define do-select-optimize
  (lambda (exp a A)
    (match exp
      (('Select (? imd? i) (? (lambda (x) (eq? x a)) r) W c)
       (let ((val (select i A))
             (new-c (do-select-optimize c a A)))
         (inc! select-optimize-count)
         `(Set ,val ,W ,new-c)))
      (('Put i r v () c)
       `(Put ,i ,r ,v () ,c))
      (('Fix B C)
       (let ((new-C (do-select-optimize C a A)))
         `(Fix ,B ,new-C)))
      (('Fix2 B C)
       (let ((new-C (do-select-optimize C a A)))
         `(Fix2 ,B ,new-C)))
      (else (Propagate do-select-optimize exp a A)))))

(define Do-Select-Optimize
  (lambda (exp)
    (match exp
      (('Cons A (w) c)
       (let* ((vec-A `(,(apply vector A) 0))
              (new-c (do-select-optimize c w vec-A)))
         `(Cons ,A (,w) ,new-c)))
      (('Vector A (w) c)
       (let* ((vec-A `(,(apply vector A) 0))
              (new-c (do-select-optimize c w vec-A)))
         `(Vector ,A (,w) ,new-c)))
      (('Stack s A (w) c)
       (let* ((vec-A `(,(apply vector A) 0))
              (new-c (do-select-optimize c w vec-A)))
         `(Stack ,s ,A (,w) ,new-c))))))

(define select-optimize
  (lambda (exp)
    (match exp
      (('Cons A (w) c)
       (if (can-put-optimize? c w)
           (select-optimize (Do-Put-Optimize exp))
           (let ((new-exp (Do-Select-Optimize exp)))
             (match new-exp
               (('Cons A (w) c)
                `(Cons ,A (,w) ,(select-optimize c)))))))
      (('Vector A (w) c)
       (if (can-put-optimize? c w)
           (select-optimize (Do-Put-Optimize exp))
           (let ((new-exp (Do-Select-Optimize exp)))
             (match new-exp
               (('Vector A (w) c)
                `(Vector ,A (,w) ,(select-optimize c)))))))
      (('Alloc (? imd? i) v (w) c)
       (let ((exp1 `(Vector ,(make-list i v) (,w) ,c)))
         (select-optimize exp1)))
      (('Stack s A (w) c)
       (let* ((new-exp (Do-Select-Optimize exp)))
         (match new-exp
           (('Stack s A (w) c) `(Stack ,s ,A (,w) ,(select-optimize c))))))
      (else (Propagate select-optimize exp)))))

(define Select-Optimize
  (lambda (program)
    (Eliminate-Set
     (select-optimize program))))

(provide "Select-Optimization")