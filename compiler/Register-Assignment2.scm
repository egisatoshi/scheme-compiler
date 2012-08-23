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

(define assign-oreg
  (lambda (exp assoc)
    (match exp
      (() (values () init-assoc))
      ((? boolean? bool) (values bool init-assoc))
      ((? integer? int) (values int init-assoc))
      ((? float? float) (values float init-assoc))
      ((? var? var) (values var init-assoc))
      (('Cons A W c)
       (let-values ((new-c new-assoc) (assign-oreg c assoc))
         (values `(Cons ,A ,W ,new-c) new-assoc)))
      (('Vector A W c)
       (let-values ((new-c new-assoc) (assign-oreg c assoc))
         (values `(Vector ,A ,W ,new-c) new-assoc)))
      (('Stack s A W c)
       (let-values ((new-c new-assoc) (assign-oreg c assoc))
         (values `(Stack ,s ,A ,W ,new-c) new-assoc)))
      (('Select i r W c)
       (let-values ((new-c new-assoc) (assign-oreg c assoc))
         (values `(Select ,i ,r ,W ,new-c) new-assoc)))
      (('Alloc i v W c)
       (let-values ((new-c new-assoc) (assign-oreg c assoc))
         (values `(Alloc ,i ,v ,W ,new-c) new-assoc)))
      (('Put i r v () c)
       (let-values ((new-c new-assoc) (assign-oreg c assoc))
         (values `(Put ,i ,r ,v () ,new-c) new-assoc)))
      (('Offset i r W c)
       (let-values ((new-c new-assoc) (assign-oreg c assoc))
         (values `(Offset ,i ,r ,W ,new-c) new-assoc)))
      (('Primop i A W c)
       (let-values ((new-c new-assoc) (assign-oreg c assoc))
         (values `(Primop ,i ,A ,W ,new-c) new-assoc)))
      (('If t tc fc) ;; essential
       (let*-values '(((new-tc assoc1) (assign-oreg tc assoc))
                      ((new-fc new-assoc) (assign-oreg fc assoc1)))
         (values `(If ,t ,new-tc ,new-fc) new-assoc)))
      ((`Apply f A) ;; essential
       (let ((try-assoc (zip A (gen-oreg-list (length A)))))
         (let*-values (((set-assoc1 midd-assoc (partision (lambda (p) (imd? (car p))) try-assoc)))
                       ((pre-set-assoc2 reg-assoc1 (partision (lambda (p)
                                                                )
                                                              midd-assoc))))
           )))
      (else (errorf "~s : no match expressin : ~s\n" "calc-oreg-assoc" exp)))))

(define Assign-oreg
  (lambda (exp)
    (let-values (((new-exp oreg-assoc) (assign-oreg exp init-assoc)))
      new-exp)))
      
(define Assign-vreg
  (lambda (exp)
    ))

(define Register-Assignment
  (lambda (program)
    (format (standard-error-port) "Register-Assignment start...\n")
    (format (standard-error-port) "function count : ~s.\n" (length (cadr program)))
    (let ((new-program (match program
                         (('Fix B C)
                          (let ((new-C (Assign-vreg (Assign-oreg C)))
                                (new-B (map (match-lambda
                                                      ((f A D)
                                                       (let* ((new-A (gen-ireg-list (length A)))
                                                              (D1 (Substitute D A new-A))
                                                              (D2 (Assign-oreg D1))
                                                              (new-D (Assign-vreg D2)))
                                                         (format (standard-error-port) ".")
                                                         `(,f ,new-A ,new-D))))
                                            B)))
                            `(Fix ,new-B ,new-C))))))
      (format (standard-error-port) "\nRegister-Assignment finished.\n")
      new-program)))

(provide "Register-Assignment")
