;;;
;;; Assoc (Library)
;;;

(define-module Assoc
  (use srfi-1)
  (export init-assoc
          get-value-cont
          get-value
          map-get-value-cont
          map-get-value
          assoc-adjoin
          assoc-extend
          ))
(select-module Assoc)

(define init-assoc '())

(define get-value-cont
  (lambda (key assoc cont error-cont)
    (if (null? assoc)
        (error-cont key)
        (if (eq? key (caar assoc))
            (cont (cadar assoc))
            (get-value-cont key (cdr assoc) cont error-cont)))))

(define get-value
  (lambda (key assoc)
    (get-value-cont key assoc (lambda (x) x) (lambda (x) x))))

(define map-get-value-cont
  (lambda (key-list assoc cont error-cont)
    (map (lambda (key) (get-value-cont key assoc cont error-cont))
         key-list)))

(define map-get-value
  (lambda (key-list assoc)
    (map-get-value-cont key-list assoc (lambda (x) x) (lambda (x) x))))

(define assoc-adjoin
  (lambda (key value assoc)
    (cons `(,key ,value) assoc)))

(define assoc-extend
  (lambda (keys values assoc)
    (append (zip keys values) assoc)))

(provide "Assoc")