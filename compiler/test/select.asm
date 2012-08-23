(;; test/select.scm
main
    (set imd s0 65536)
    (set imd s1 1048575)
    (set reg v0 s0)
    (+ imd s0 s0 3)
    (set imd h0 1)
    (store h0 v0 0)
    (set imd h0 2)
    (store h0 v0 1)
    (set imd h0 3)
    (store h0 v0 2)
    (halt)
)
