;;;
;;; Register-Assignment
;;;

(define-module Register-Assignment
  (use srfi-1)
  (use srfi-11)
  (use util.match)
  (require "./Basic-Utility")
  (require "./Id")
  (require "./Set")
  (require "./Free-Variable")
  (require "./Substitution")
  (require "./Globals")
  (import Basic-Utility)
  (import Id)
  (import Set)
  (import Free-Variable)
  (import Substitution)
  (import Globals)
  (export Register-Assignment
          ))
(select-module Register-Assignment)

(define register-pool
  '(v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
    v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
    v20 v21 v22 v23 v24 v25 v26 v27 v28 v29
    v30 v31 v32 v33 v34 v35 v36 v37 v38 v39
    v40 v41 v42 v43 v44 v45 v46 v47 v48 v49
    v50 v51 v52 v53 v54 v55 v56 v57 v58 v59
    v60 v61 v62 v63 v64 v65 v66 v67 v68 v69
    v70 v71 v72 v73 v74 v75 v76 v77 v78 v79
    v80 v81 v82 v83 v84 v85 v86 v87 v88 v89
    v91 v91 v92 v93 v94 v95
    ))

(define get-new-register
  (lambda (Va U P)
    (let ((D (set-diff U Va)))
      (if (null? D)
          (let* ((new-w (car P))
                 (new-P (cdr P))
                 (new-U (set-adjoin U new-w)))
            (values new-w new-U new-P))
          (let ((new-w (car D)))
            (values new-w U P))))))

(define insert-Set
  (lambda (A new-A exp)
    (if (null? A)
        exp
        `(Set ,(car A) (,(car new-A))
              ,(insert-Set (cdr A) (cdr new-A) exp)))))

(define imd?
  (lambda (x)
    (or (not (var? x))
        (let ((s (x->string x)))
          (char=? (string-ref s 0) #\L)))))

(define extract-vars
  (lambda (A)
    (filter (lambda (x) (not (imd? x))) A)))

(define register-assignment
  (lambda (U P exp)
    (match exp
      (() ())
      ((? boolean? bool) bool)
      ((? integer? int) int)
      ((? float? float) float)
      ((? var? var) var)
      (('Cons A ((? not-global? w)) c)
       (let ((Va (set-union (Free-Variable c) (extract-vars A))))
         (let-values (((new-w new-U new-P) (get-new-register Va U P)))
           (let ((new-c (Substitute c `(,w) `(,new-w))))
             `(Cons ,A (,new-w) ,(register-assignment new-U new-P new-c))))))
      (('Cons A ((? global? w)) c) ;; global
       `(Cons ,A (,w) ,(register-assignment U P c)))
      (('Vector A ((? not-global? w)) c)
       (let ((Va (set-union (Free-Variable c) (extract-vars A))))
         (let-values (((new-w new-U new-P) (get-new-register Va U P)))
           (let ((new-c (Substitute c `(,w) `(,new-w))))
             `(Vector ,A (,new-w) ,(register-assignment new-U new-P new-c))))))
      (('Vector A ((? global? w)) c)
       `(Vector ,A (,w) ,(register-assignment U P c)))
      (('Stack s A ((? not-global? w)) c)
       (let ((Va (set-union (Free-Variable c) (extract-vars A) `(,s)))) ;; Really nead - `(,s) ?
         (let-values (((new-w new-U new-P) (get-new-register Va U P)))
           (let ((new-c (Substitute c `(,w) `(,new-w))))
             `(Stack ,s ,A (,new-w) ,(register-assignment new-U new-P new-c))))))
      (('Stack s A ((? global? w)) c)
       `(Stack ,s ,A (,w) ,(register-assignment U P c)))
      (('Select i A ((? not-global? w)) c)
       (let ((Va (Free-Variable c)))
         (let-values (((new-w new-U new-P) (get-new-register Va U P)))
           (let ((new-c (Substitute c `(,w) `(,new-w))))
             `(Select ,i ,A (,new-w) ,(register-assignment new-U new-P new-c))))))
      (('Select i A ((? global? w)) c)
       `(Select ,i ,A (,w) ,(register-assignment U P c)))
      (('Alloc i v ((? not-global? w)) c)
       (let ((Va (set-union (Free-Variable c) (extract-vars (list i v)))))
         (let-values (((new-w new-U new-P) (get-new-register Va U P)))
           (let ((new-c (Substitute c `(,w) `(,new-w))))
             `(Alloc ,i ,v (,new-w) ,(register-assignment new-U new-P new-c))))))
      (('Alloc i v ((? global? w)) c)
       `(Alloc ,i ,v (,w) ,(register-assignment U P c)))
      (('Put i A v () c)
       `(Put ,i ,A ,v () ,(register-assignment U P c)))
      (('Offset i A ((? not-global? w)) c)
       (let ((Va (Free-Variable c)))
         (let-values (((new-w new-U new-P) (get-new-register Va U P)))
           (let ((new-c (Substitute c `(,w) `(,new-w))))
             `(Offset ,i ,A (,new-w) ,(register-assignment new-U new-P new-c))))))
      (('Offset i A ((? global? w)) c)
       `(Offset ,i ,A (,w) ,(register-assignment U P c)))
      (('Primop i A W c)
       (match W
         (()
          `(Primop ,i ,A () ,(register-assignment U P c)))
         (((? not-global? w))
          (let ((Va (Free-Variable c)))
            (let-values (((new-w new-U new-P) (get-new-register Va U P)))
              (let ((new-c (Substitute c `(,w) `(,new-w))))
                `(Primop ,i ,A (,new-w) ,(register-assignment new-U new-P new-c))))))
         (((? global? w))
          `(Primop ,i ,A (,w) ,(register-assignment U P c)))))
      (('If t tc fc)
       (let ((new-tc (register-assignment U P tc))
             (new-fc (register-assignment U P fc)))
         `(If ,t ,new-tc ,new-fc)))
      (('Apply f A)
       (let ((new-A (gen-oreg-list (length A))))
         (insert-Set A new-A `(Apply ,f ,new-A))))
      (else "error at register-assignment"))))

(define Register-Assignment
  (lambda (program)
    (format (standard-error-port) "Register-Assignment start...\n")
    (format (standard-error-port) "function count : ~s.\n" (length (cadr program)))
    (let ((new-program (match program
                         (('Fix bound-list A)
                          (let ((new-A (register-assignment '() register-pool A))
                                (new-bound-list (map (match-lambda
                                                      ((f A B)
                                                       (let* ((new-A (gen-ireg-list (length A)))
                                                              (new-B (Substitute B A new-A))
                                                              (new-new-B (register-assignment '() register-pool new-B)))
                                                         (format (standard-error-port) ".")
                                                         `(,f ,new-A ,new-new-B))))
                                                     bound-list)))
                            `(Fix ,new-bound-list ,new-A))))))
      (format (standard-error-port) "\nRegister-Assignment finished.\n")
      new-program)))

(provide "Register-Assignment")
