[![Build
Status](https://travis-ci.org/esmolanka/simple-pi.svg?branch=master)](https://travis-ci.org/esmolanka/simple-pi)

Experiments with dependently typed lambda calculus.

```
$ spi << EOF

(Definition numeral
  (forall (A :: *) -> (-> (-> A A) A A)))

(Definition zero
  (lambda ((A :: *) (f :: (-> A A)) (x :: A))
    x))

(Definition one
  (lambda ((A :: *) (f :: (-> A A)) (x :: A))
    (f x)))

(Definition two
  (lambda ((A :: *) (f :: (-> A A)) (x :: A))
    (f (f x))))

(Definition three
  (lambda ((A :: *) (f :: (-> A A)) (x :: A))
    (f (f (f x)))))

(Parameter Nat *)
(Parameter z Nat)
(Parameter s (-> Nat Nat))

(Definition plus
  (lambda ((m :: numeral) (n :: numeral) (A :: *) (f :: (-> A A)) (x :: A))
    (m A f (n A f x))))

(Eval (plus three two Nat s z))

EOF

(s (s (s (s (s z)))))
```
