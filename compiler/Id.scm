(define-module Id
  (use util.match)
  (require "./Basic-Utility")
  (require "./Assoc")
  (import Basic-Utility)
  (import Assoc)
  (export gentmpf
          gentmpf-list
          gentmpr
          gentmpr-list
          gentmpv
          gentmpv-list
          gen-record
          gen-ireg-list
          gen-oreg-list
          gen-label
          gen-label-list
          gen-alloc-label
          gen-if-label
          ))
(select-module Id)

(define gentmpf
  (let ((n 0))
    (lambda ()
      (inc! n)
      (append-to-symbol "f" n))))

(define gentmpf-list
  (lambda (n)
    (if (= n 0)
        '()
        (cons (gentmpf) (gentmpf-list (- n 1))))))

(define gentmpr
  (let ((n 0))
    (lambda ()
      (inc! n)
      (append-to-symbol "r" n))))

(define gentmpr-list
  (lambda (n)
    (if (= n 0)
        '()
        (cons (gentmpr) (gentmpr-list (- n 1))))))

(define gentmpv
  (let ((n 0))
    (lambda ()
      (inc! n)
      (append-to-symbol "t" n))))

(define gentmpv-list
  (lambda (n)
    (if (= n 0)
        '()
        (cons (gentmpv) (gentmpv-list (- n 1))))))

(define gen-record
  (let ((n 0))
    (lambda ()
      (inc! n)
      (append-to-symbol "R" n))))

(define gen-ireg-list
  (lambda (n)
    (letrec ((gen-ireg (lambda (n)
                         (let* ((n-str (number->string n))
                                (ireg (string-append "i" n-str)))
                           (string->symbol ireg)))))
             (let loop ((i 0))
               (if (= i n)
                   '()
                   (cons (gen-ireg i) (loop (+ i 1))))))))

(define gen-oreg-list
  (lambda (n)
    (letrec ((gen-oreg  (lambda (n)
                          (let* ((n-str (number->string n))
                                 (oreg (string-append "o" n-str)))
                            (string->symbol oreg)))))
      (let loop ((i 0))
        (if (= i n)
            '()
            (cons (gen-oreg i) (loop (+ i 1))))))))

(define gen-label
  (let ((n 0))
    (lambda ()
      (inc! n)
      (append-to-symbol "L" n))))

(define gen-label-list
  (lambda (n)
    (if (= n 0)
        '()
        (cons (gen-label) (gen-label-list (- n 1))))))

(define gen-alloc-label
  (let ((n 0))
    (lambda ()
      (inc! n)
      (append-to-symbol "Alloc" n))))

(define gen-if-label
  (let ((n 0))
    (lambda ()
      (inc! n)
      (append-to-symbol "If" n))))

(provide "Id")