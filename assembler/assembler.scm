#!/usr/bin/gosh
;;;
;;; Assembler
;;;

; === Instruction Set ===

; see design.txt

(use gauche.parseopt)
(use util.match)
(define debug-option #f)
(define simulator-option #f)

;; config

(define power
  (lambda (e n)
    (cond
     ((= n 0) 1)
     ((< n 0)
      (/ 1 (power e (- 0 n)))
      )
     (else (* e (power e (- n 1)))))))

(define start-textseg 0)  ; 2 ^ 16 bit = 2 ^ 10 word

(define program (make-vector (power 2 19) 0))

;; Utility

(define label-table (make-hash-table))

(define dummy-reg 0)
(define dummy-imd 0)

(define write-byte-for-debug ; for debug
  (lambda (n)
    (format #t "~8,'0B " n)))

(define write-byte-for-simulator ; for simulator
  (lambda (n)
    (format #t "~8,'0B" n)))

(define write-word
  (lambda (n)
    (let ((b3 (/ (logand n #xff000000) #x01000000))
          (b2 (/ (logand n #x00ff0000) #x00010000))
          (b1 (/ (logand n #x0000ff00) #x00000100))
          (b0 (logand n #x000000ff)))
      (cond
       (debug-option
        (begin
          (write-byte-for-debug b3)
          (write-byte-for-debug b2)
          (write-byte-for-debug b1)
          (write-byte-for-debug b0)))
       (simulator-option
        (begin
          (write-byte-for-simulator b3)
          (write-byte-for-simulator b2)
          (write-byte-for-simulator b1)
          (write-byte-for-simulator b0)))
       (else
        (begin
          (write-byte b3)
          (write-byte b2)
          (write-byte b1)
          (write-byte b0)))
        ))))

(define write-opeword
  (lambda (opecode reg1 reg2 reg3 imd)
    (cond
     (debug-option
      (begin
        (write-byte-for-debug opecode)
        (write-byte-for-debug reg1)
        (write-byte-for-debug reg2)
        (write-byte-for-debug reg3)
        (newline)
        (write-word imd)
        (newline)))
     (simulator-option
      (begin
        (write-byte-for-simulator opecode)
        (write-byte-for-simulator reg1)
        (write-byte-for-simulator reg2)
        (write-byte-for-simulator reg3)
        (newline)
        (write-word imd)
        (newline)))
     (else
      (begin
        (write-byte opecode)
        (write-byte reg1)
        (write-byte reg2)
        (write-byte reg3)
        (write-word imd))))))

;;;
;;; Helper (reg-set!, reg-ref, get-address, print-registers)
;;;

(define reg-get
  (lambda (reg-name)
    (let* ((reg-name-str (symbol->string reg-name))
           (reg-prefix (string-ref reg-name-str 0))
           (reg-suffix (string-copy reg-name-str 1))
           (reg-num (string->number reg-suffix)))
      (cond
       ((char=? reg-prefix #\i) (+ 0 reg-num))
       ((char=? reg-prefix #\o) (+ 32 reg-num))
       ((char=? reg-prefix #\g) (+ 64 reg-num))
       ((char=? reg-prefix #\v) (+ 128 reg-num))
       ((char=? reg-prefix #\h) (+ 224 reg-num))
       ((char=? reg-prefix #\s) (+ 240 reg-num))
       (else (write reg-name) (print " : reg-get error"))))))

(define get-address
  (lambda (label)
    (* 2 (hash-table-get label-table label))))

;;;
;;; Asm Helper
;;;

(define make-e
  (lambda (decimal n)
    (if (> 2 (/ decimal (power 2 n)))
        n
        (make-e decimal (+ n 1)))))

(define flsub
  (lambda (decimal)
    (* (power 2 23) (- (/ decimal (power 2 (make-e decimal (- 0 127)))) 1))))

(define convert-to-float
  (lambda (decimal)
    (if (= decimal 0.0)
        0
        (let* ((s (if (> decimal 0)
                      0
                      1))
               (num (* (abs decimal) (power 2 127)))
               (e (make-e num (- 0 127)))
               (f (flsub num))
               (ft (inexact->exact (round f))))
          (+ (* s (power 2 31))
             (* e (power 2 23))
             ft)))))

(define label? symbol?)

(define float? inexact?)

(define convert-to-bits
  (lambda (imd)
    (match imd
      (() 0)
      (#t #xffffffff)
      (#f 0)
      ((? label? imd) (get-address imd))
      ((? float? imd) (convert-to-float imd))
      (else imd))))

;;;
;;; Core of the Simulator
;;;

(define run-asm-iter
  (lambda (c)
    (let ((exp (vector-ref program c)))
      (match exp
        (('nop)
         (write-opeword #b00000000
                        dummy-reg
                        dummy-reg
                        dummy-reg
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('halt)
         (write-opeword #b00000001
                        dummy-reg
                        dummy-reg
                        dummy-reg
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('read-byte dr)
         (write-opeword #b00100000
                        (reg-get dr)
                        dummy-reg
                        dummy-reg
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('read-word dr)
         (write-opeword #b00100001
                        (reg-get dr)
                        dummy-reg
                        dummy-reg
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('write-byte 'reg sr2)
         (write-opeword #b00100010
                        dummy-reg
                        dummy-reg
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('write-byte 'imd imd)
         (write-opeword #b00110010
                        dummy-reg
                        dummy-reg
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))
        
        (('write-word 'reg sr2)
         (write-opeword #b00100011
                        dummy-reg
                        dummy-reg
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('write-word 'imd imd)
         (write-opeword #b00110011
                        dummy-reg
                        dummy-reg
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('load 'reg dr sr1 sr2)
         (write-opeword #b01010000
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('load 'imd dr sr1 imd)
         (write-opeword #b01010001
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('store sr2 sr1 imd)
         (write-opeword #b01010010
                        dummy-reg
                        (reg-get sr1)
                        (reg-get sr2)
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('if 'reg sr2)
         (write-opeword #b01100000
                        dummy-reg
                        dummy-reg
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('if 'imd imd)
         (write-opeword #b01110000
                        dummy-reg
                        dummy-reg
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))
        
        (('goto 'reg sr2)
         (write-opeword #b01100001
                        dummy-reg
                        dummy-reg
                        (get-address (reg-get sr2))
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('goto 'imd imd)
         (write-opeword #b01110001
                        dummy-reg
                        dummy-reg
                        dummy-reg
                        (get-address imd))
         (run-asm-iter (+ c 1)))

        (('call 'reg sr2)
         (write-opeword #b01100010
                        dummy-reg
                        dummy-reg
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('call 'imd imd)
         (write-opeword #b01110010
                        dummy-reg
                        dummy-reg
                        dummy-reg
                        (get-address imd))
         (run-asm-iter (+ c 1)))

        (('= 'reg dr sr1 sr2)
         (write-opeword #b10000000
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('= 'imd dr sr1 imd)
         (write-opeword #b10010000
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('=. 'reg dr sr1 sr2)
         (write-opeword #b10000001
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('=. 'imd dr sr1 imd)
         (write-opeword #b10010001
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('> 'reg dr sr1 sr2)
         (write-opeword #b10000010
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('> 'imd dr sr1 imd)
         (write-opeword #b10010010
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('>. 'reg dr sr1 sr2)
         (write-opeword #b10000011
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('>. 'imd dr sr1 imd)
         (write-opeword #b10010011
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('< 'reg dr sr1 sr2)
         (write-opeword #b10000100
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('< 'imd dr sr1 imd)
         (write-opeword #b10010100
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('<. 'reg dr sr1 sr2)
         (write-opeword #b10000101
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('<. 'imd dr sr1 imd)
         (write-opeword #b10010101
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('>= 'reg dr sr1 sr2)
         (write-opeword #b10000110
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('>= 'imd dr sr1 imd)
         (write-opeword #b10010110
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('>=. 'reg dr sr1 sr2)
         (write-opeword #b10000111
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('>=. 'imd dr sr1 imd)
         (write-opeword #b10010111
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('<= 'reg dr sr1 sr2)
         (write-opeword #b10001000
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('<= 'imd dr sr1 imd)
         (write-opeword #b10011000
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('<=. 'reg dr sr1 sr2)
         (write-opeword #b10001001
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('<=. 'imd dr sr1 imd)
         (write-opeword #b10011001
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('set 'reg dr sr2)
         (write-opeword #b10100000
                        (reg-get dr)
                        dummy-reg
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('set 'imd dr imd)
         (write-opeword #b10110000
                        (reg-get dr)
                        dummy-reg
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('+ 'reg dr sr1 sr2)
         (write-opeword #b10100001
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('+ 'imd dr sr1 imd)
         (write-opeword #b10110001
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('- 'reg dr sr1 sr2)
         (write-opeword #b10100010
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('- 'imd dr sr1 imd)
         (write-opeword #b10110010
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('xor 'reg dr sr1 sr2)
         (write-opeword #b10100011
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('xor 'imd dr sr1 imd)
         (write-opeword #b10110011
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))
        
        (('or 'reg dr sr1 sr2)
         (write-opeword #b10100100
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('or 'imd dr sr1 imd)
         (write-opeword #b10110100
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))
        
        (('and 'reg dr sr1 sr2)
         (write-opeword #b10100101
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('and 'imd dr sr1 imd)
         (write-opeword #b10110101
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('not 'reg dr sr2)
         (write-opeword #b10100110
                        (reg-get dr)
                        dummy-reg
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('not 'imd dr imd)
         (write-opeword #b10110110
                        (reg-get dr)
                        dummy-reg
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('sll 'reg dr sr1 sr2)
         (write-opeword #b11000000
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('sll 'imd dr sr1 imd)
         (write-opeword #b11010000
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('srl 'reg dr sr1 sr2)
         (write-opeword #b11000001
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr3)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('srl 'imd dr sr1 imd)
         (write-opeword #b11010001
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('* 'reg dr sr1 sr2)
         (write-opeword #b11000010
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('* 'imd dr sr1 imd)
         (write-opeword #b11010010
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('/ 'reg dr sr1 sr2)
         (write-opeword #b11000011
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('/ 'imd dr sr1 imd)
         (write-opeword #b11010011
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('modulo 'reg dr sr1 sr2)
         (write-opeword #b11000100
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('modulo 'imd dr sr1 imd)
         (write-opeword #b11010100
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('+. 'reg dr sr1 sr2)
         (write-opeword #b11000101
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('+. 'imd dr sr1 imd)
         (write-opeword #b11010101
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('-. 'reg dr sr1 sr2)
         (write-opeword #b11000110
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('-. 'imd dr sr1 imd)
         (write-opeword #b11010110
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('*. 'reg dr sr1 sr2)
         (write-opeword #b11000111
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('*. 'imd dr sr1 imd)
         (write-opeword #b11010111
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('/. 'reg dr sr1 sr2)
         (write-opeword #b11001000
                        (reg-get dr)
                        (reg-get sr1)
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))
        
        (('/. 'imd dr sr1 imd)
         (write-opeword #b11011000
                        (reg-get dr)
                        (reg-get sr1)
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('itof 'reg dr sr2)
         (write-opeword #b11001001
                        (reg-get dr)
                        dummy-reg
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('itof 'imd dr imd)
         (write-opeword #b11011001
                        (reg-get dr)
                        dummy-reg
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (('ftoi 'reg dr sr2)
         (write-opeword #b11001010
                        (reg-get dr)
                        dummy-reg
                        (reg-get sr2)
                        dummy-imd)
         (run-asm-iter (+ c 1)))

        (('ftoi 'imd dr imd)
         (write-opeword #b11001010
                        (reg-get dr)
                        dummy-reg
                        dummy-reg
                        (convert-to-bits imd))
         (run-asm-iter (+ c 1)))

        (0 'end) ;; end of program
        
        (else (display (car exp)) (print "unknowon instruction"))))))

(define run-asm
  (lambda ()
    (run-asm-iter start-textseg)))

(define load-program-iter
  (lambda (program-list c)
    (if (null? program-list)
        'done
        (let ((exp (car program-list)))
          (if (pair? exp)
              (begin
                (vector-set! program c (car program-list))
                (load-program-iter (cdr program-list) (+ c 1)))
              (begin
                (hash-table-put! label-table (car program-list) c)
                (load-program-iter (cdr program-list) c)))))))

(define load-program
  (lambda (program-list)
    (load-program-iter program-list start-textseg)))

(define main
  (lambda (args)
    (let-args (cdr args)
              ((debug "d|debug")
               (simulator "s|simulator")
               . restargs
               )
              (set! debug-option debug)
              (set! simulator-option simulator)
              (load-program (read (open-input-file (car restargs))))
              (run-asm))))




