(letrec ((fib (lambda (n)
                (if (< n 3) 1
                    (let ((a (fib (- n 1)))
                          (b (fib (- n 2))))
                      (+ a b))))))
  (write-byte (fib 10)))