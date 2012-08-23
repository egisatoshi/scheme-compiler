;;;
;;; Set
;;;

(define-module Set
  (use srfi-1)
  (export empty-set
          singleton
          set-adjoin
          set-union
          set-intersection
          set-delete
          set-diff
          set-member?
          ))
(select-module Set)

(define empty-set '())

(define singleton
  (lambda (x)
    (list x)))

(define set-adjoin
  (lambda (set x)
    (lset-adjoin eq? set x)))

(define set-union
  (lambda sets
    (apply lset-union (cons eq? sets))))

(define set-intersection
  (lambda sets
    (apply lset-intersection (cons eq? sets))))

(define set-delete
  (lambda (set x)
    (lset-difference eq? set (singleton x))))

(define set-diff
  (lambda (a-set . b-sets)
    (if (null? b-sets)
        a-set
        (apply set-diff (cons (lset-difference eq? a-set (car b-sets)) (cdr b-sets))))))

(define set-member?
  (lambda (x set)
    (if (eq? (memq x set) #f)
        #f
        #t)))

(provide "Set")

