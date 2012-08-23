(letrec ((odd? (lambda (n)
                 (if (= n 0)
                     0
                     (even? (- n 1)))))
         (even? (lambda (n)
                  (if (= n 0)
                      1
                      (odd? (- n 1))))))
  (vector (even? 13)
          (even? 12)
          (odd? 2)
          (odd? 3)))