(define-module Basic-Utility
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (export var?
          imd?
          float?
          exists?
          findn
          member-count
          companion-filter
          append-to-symbol
          make-list
          ))
(select-module Basic-Utility)

;;
;; Basic Utility
;;

(define var? symbol?)

(define imd? (lambda (x) (not (var? x))))

(define float?
  (lambda (val)
    (and (real? val) (not (integer? val)))))

(define exists?
  (lambda (x ls)
    (if (null? ls)
        #f
        (if (eq? x (car ls))
            #t
            (exists? x (cdr ls))))))

(define findn
  (lambda (x ls)
    (let loop ((ls ls)
               (n 0))
      (if (null? ls)
          -1
          (if (equal? x (car ls))
              n
              (loop (cdr ls) (+ n 1)))))))

(define member-count
  (lambda (x ls)
    (let loop ((ls ls)
               (n 0))
      (if (null? ls)
          n
          (if (equal? x (car ls))
              (loop (cdr ls) (+ n 1))
              (loop (cdr ls) n))))))

(define companion-filter
  (lambda (pred V C)
    (if (null? V)
        (values '() '())
        (let-values (((Va Ca) (companion-filter pred (cdr V) (cdr C))))
          (if (pred (car V))
              (values (cons (car V) Va) (cons (car C) Ca))
              (values Va Ca))))))

(define append-to-symbol
  (lambda (x y)
    (string->symbol (string-append (x->string x) (x->string y)))))

(define make-list
  (lambda (i v)
    (if (<= i 0)
        '()
        (cons v (make-list (- i 1) v)))))

(provide "Basic-Utility")