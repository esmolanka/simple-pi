[![Build
Status](https://travis-ci.org/esmolanka/simple-pi.svg?branch=master)](https://travis-ci.org/esmolanka/simple-pi)

Experiments with dependently typed lambda calculus.

~~~
Nat  : Type.
Zero : Nat.
Succ : Nat -> Nat.

numeral : Type₁.
numeral = (A : Type) -> (A -> A) -> A -> A.

zero : numeral.
zero = \A f x => x.

one : numeral.
one = \A f x => f x.

two : numeral.
two = \A f x => f (f x).

three : numeral.
three = \A f x => f (f (f x)).

plus : numeral -> numeral -> numeral.
plus =
  \m n =>
    \A f x =>
      m A f (n A f x).

Check plus.
-- (-> (forall (A/142 :: (type 0))
--        ->
--        (-> (-> A/142 A/142) A/142 A/142))
--    (forall (A/179 :: (type 0))
--       ->
--       (-> (-> A/179 A/179) A/179 A/179))
--    (forall (A :: (type 0)) -> (-> (-> A A) A A)))

Eval plus three two Nat Succ Zero.
-- (Succ (Succ (Succ (Succ (Succ Zero)))))
-- : Nat
~~~

`Nat` addition implemented with dependent eliminator:

~~~
Nat : Type.
Zero : Nat.
Succ : Nat -> Nat.

natElim : (m : Nat -> Type)
       -> m Zero
       -> ((l : Nat) -> m l -> m (Succ l))
       -> ((k : Nat) -> m k).

ePlus : Nat -> Nat -> Nat.
ePlus =
  natElim (\ (_ : Nat) => Nat -> Nat)
          (\ (n : Nat) => n)
          (\ (k : Nat) (rec : Nat -> Nat) (n : Nat) => Succ (rec n)).

Check ePlus.
~~~
