[![Build
Status](https://travis-ci.org/esmolanka/simple-pi.svg?branch=master)](https://travis-ci.org/esmolanka/simple-pi)

Experiments with dependently typed lambda calculus.

~~~{.lisp}
(Parameter Nat Type)
(Parameter Zero Nat)
(Parameter Succ (-> Nat Nat))

(Definition numeral (forall (A :: Type) -> (-> (-> A A) A A)) :: Type₁)

(Definition zero  (lambda (A f x) x) :: numeral)
(Definition one   (lambda (A f x) (f x)) :: numeral)
(Definition two   (lambda (A f x) (f (f x))) :: numeral)
(Definition three (lambda (A f x) (f (f (f x)))) :: numeral)

(Definition plus  (lambda (m n A f x) (m A f (n A f x)))
               :: (-> numeral numeral numeral))

(Check plus)
;; =
;; (-> (forall (A/142 :: (type 0))
;;        ->
;;        (-> (-> A/142 A/142) A/142 A/142))
;;    (forall (A/179 :: (type 0))
;;       ->
;;       (-> (-> A/179 A/179) A/179 A/179))
;;    (forall (A :: (type 0)) -> (-> (-> A A) A A)))

(Eval (plus three two Nat Succ Zero))
;; =
;; (Succ (Succ (Succ (Succ (Succ Zero)))))
~~~
