(call/cc (lambda (cc)
           (+ 1 (throw cc (list 2)))))