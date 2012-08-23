;;;
;;; Dead-Variable-Elimination
;;;

(define-module Dead-Variable-Elimination
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Id")
  (require "./Propagation")
  (require "./Substitution")
  (require "./Globals")
  (require "./Unused-Function-Elimination")
  (import Basic-Utility)
  (import Id)
  (import Propagation)
  (import Substitution)
  (import Globals)
  (import Unused-Function-Elimination)
  (export dead-variable-eliminate-count
          Eliminate-Dead-Variable
          ))
(select-module Dead-Variable-Elimination)

(define dead-variable-eliminate-count 0)

(define eliminate-dead-variable
  (lambda (exp)
    (match exp
      (('Cons A ((? not-global? w)) c)
       (let ((new-c (eliminate-dead-variable c)))
         (if (> (occur-count w c) 0)
             `(Cons ,A (,w) ,new-c)
             (begin
               (inc! dead-variable-eliminate-count)
               new-c))))
      (('Vector A ((? not-global? w)) c)
       (let ((new-c (eliminate-dead-variable c)))
         (if (> (occur-count w c) 0)
             `(Vector ,A (,w) ,new-c)
             (begin
               (inc! dead-variable-eliminate-count)
               new-c))))
      (('Stack s A ((? not-global? w)) c)
       (let ((new-c (eliminate-dead-variable c)))
         (if (> (occur-count w c) 0)
             `(Stack ,s ,A (,w) ,new-c)
             (begin
               (inc! dead-variable-eliminate-count)
               new-c))))
      (('Select i r ((? not-global? w)) c)
       (let ((new-c (eliminate-dead-variable c)))
         (if (> (occur-count w c) 0)
             `(Select ,i ,r (,w) ,new-c)
             (begin
               (inc! dead-variable-eliminate-count)
               new-c))))
      (('Alloc i v ((? not-global? w)) c)
       (let ((new-c (eliminate-dead-variable c)))
         (if (> (occur-count w c) 0)
             `(Alloc ,i ,v (,w) ,new-c)
             (begin
               (inc! dead-variable-eliminate-count)
               new-c))))
      (('Offset i r ((? not-global? w)) c)
       (let ((new-c (eliminate-dead-variable c)))
         (if (> (occur-count w c) 0)
             `(Offset ,i ,r (,w) ,new-c)
             (begin
               (inc! dead-variable-eliminate-count)
               new-c))))
      (('Primop i A ((? not-global? w)) c)
       (let ((new-c (eliminate-dead-variable c)))
         (if (> (occur-count w c) 0)
             `(Primop ,i ,A (,w) ,new-c)
             (begin
               (inc! dead-variable-eliminate-count)
               new-c))))
      (('Set v ((? not-global? w)) c)
       (let ((new-c (eliminate-dead-variable c)))
         (if (> (occur-count w c) 0)
             `(Set ,v (,w) ,new-c)
             (begin
               (inc! dead-variable-eliminate-count)
               new-c))))
      (else (Propagate eliminate-dead-variable exp)))))

(define Eliminate-Dead-Variable
  (lambda (program)
;    (format (standard-error-port) "Eliminate-Dead-Variable start...\n")
    (let ((new-program (eliminate-dead-variable program)))
;      (format (standard-error-port) "Eliminate-Dead-Variable finished.\n")
;      (format (standard-error-port) "Dead-Variable-Eliminate-Count : ~s.\n" dead-variable-eliminate-count)
      new-program)))

(provide "Dead-Variable-Elimination")
