#!/usr/bin/gosh
;;;
;;; Scheme Compiler
;;;

(use gauche.parseopt)
(require "./Basic-Utility")
(require "./Object-Language")
(require "./CPS-Language")
(require "./Assoc")
(require "./Id")
(require "./Globals")
(require "./Alpha-Transformation")
(require "./Unused-Function-Elimination")
(require "./Measure-Program-Size")
(require "./CPS-Conversion")
(require "./Eta-Reduction")
(require "./Beta-Contraction")
(require "./Select-Optimization")
(require "./Primop-Optimization")
(require "./If-Optimization")
(require "./Beta-Expansion")
(require "./Closure-Conversion")
(require "./Register-Assignment")
(import Basic-Utility)
(import Object-Language)
(import CPS-Language)
(import Assoc)
(import Id)
(import Globals)
(import Alpha-Transformation)
(import Unused-Function-Elimination)
(import Measure-Program-Size)
(import CPS-Conversion)
(import Eta-Reduction)
(import Beta-Contraction)
(import Select-Optimization)
(import Primop-Optimization)
(import If-Optimization)
(import Beta-Expansion)
(import Closure-Conversion)
(import Register-Assignment)

(define Constant-Folding
  (lambda (program)
    (letrec ((constant-folding (lambda (program)
                                 (If-Optimize
                                  (Primop-Optimize
                                   (Select-Optimize
                                    program))))))
      (let ((opt-program (constant-folding program)))
        (if (equal? opt-program program)
            program
            (Constant-Folding opt-program))))))

(define Optimize
  (lambda (program)
    (Constant-Folding
;     (Beta-Expand
      program)))

(define iterate-count 10)
(define init-size 0)

(define Iterate-Optimize
  (lambda (program)
    (set! init-size (Measure-Program-Size program))
    (let loop ((program program)
               (count 1))
      (let ((opt-program (Optimize program)))
        (if (or (> count iterate-count)
                (> (Measure-Program-Size opt-program) 20000)
                (equal? opt-program program))
            (begin
              (format (standard-error-port) "Select-Optimize-Count : ~s.\n" select-optimize-count)
              (format (standard-error-port) "If-Optimize-Count : ~s.\n" if-optimize-count)
              (format (standard-error-port) "Primop-Optimize-Count : ~s.\n" primop-optimize-count)
              program)
            (begin
              (format (standard-error-port) "~s-th Optimize phase iterated\n" count)
              (loop opt-program (+ count 1))))))))

(define compile-c
  (lambda (program)
    (Closure-Convert
     (Iterate-Optimize
      (Constant-Folding
       (Beta-Contract
        (Eta-Reduct
         (CPS-Convert
          program))))))))

(define compile-b
  (lambda (program)
    (Register-Assignment
     (Closure-Convert
      (Iterate-Optimize
       (Constant-Folding
        (Beta-Contract
         (Eta-Reduct
          (CPS-Convert
           program)))))))))

(define main
  (lambda (args)
    (let ((file-name (cadr args)))
      (call-with-input-file file-name
        (lambda (input)
          (let* ((program (read input))
                 (compiled-program (compile-b program))
                 )
            (eval compiled-program interaction-environment)))))))
