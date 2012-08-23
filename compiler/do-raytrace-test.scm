#!/usr/bin/gosh
;;;
;;; Scheme Compiler
;;;

(use gauche.parseopt)
(require "./Basic-Utility")
(require "./Object-Language")
;(require "./CPS-Language")
;(require "./CPS-Conversion")
(import Basic-Utility)
(import Object-Language)
;(import CPS-Language)
;(import CPS-Conversion)

(define compile-b
  (lambda (program)
;    (CPS-Convert
     program))

(define main
  (lambda (args)
    (let ((file-name (cadr args)))
      (call-with-input-file file-name
        (lambda (input)
          (let* ((program (read input))
                 (compiled-program (compile-b program))
                 )
            (eval compiled-program interaction-environment)))))))
