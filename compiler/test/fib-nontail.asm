(;; test/fib-nontail.scm
main
    (set imd s0 524288)
    (set imd s1 1048575)
    (- imd v0 s1 1)
    (set imd h0 L4)
    (store h0 v0 0)
    (set reg o0 v0)
    (set imd o1 10)
    (call imd L1)
L1
    (< imd v0 i1 3)
    (if reg v0)
    (goto imd If1)
    (goto imd If2)
If1
    (load imd v0 i0 0)
    (set reg o0 i0)
    (set imd o1 1)
    (call reg v0)
If2
    (- imd v0 i0 3)
    (set imd h0 L2)
    (store h0 v0 0)
    (store i1 v0 1)
    (store i0 v0 2)
    (- imd v1 i1 1)
    (set reg o0 v0)
    (set reg o1 v1)
    (call imd L1)
L2
    (load imd v0 i0 1)
    (load imd v1 i0 2)
    (- imd v2 i0 3)
    (set imd h0 L3)
    (store h0 v2 0)
    (store v1 v2 1)
    (store i1 v2 2)
    (- imd v1 v0 2)
    (set reg o0 v2)
    (set reg o1 v1)
    (call imd L1)
L3
    (load imd v0 i0 1)
    (load imd v1 i0 2)
    (+ reg v1 v1 i1)
    (load imd v2 v0 0)
    (set reg o0 v0)
    (set reg o1 v1)
    (call reg v2)
L4
    (write-byte reg i1)
    (halt)
)
