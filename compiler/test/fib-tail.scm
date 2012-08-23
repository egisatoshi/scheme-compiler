(letrec ((fib (lambda (n d1 d2)
                (if (< n 3)
                    d1
                    (fib (- n 1) (+ d1 d2) d1)))))
  (fib 10 1 1))