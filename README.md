[![Build
Status](https://travis-ci.org/esmolanka/simple-pi.svg?branch=master)](https://travis-ci.org/esmolanka/simple-pi)

Experiments with dependently typed lambda calculus.

~~~
Nat  : Type
Zero : Nat
Succ : Nat -> Nat

numeral : Type₁
numeral = ∀ (A : Type) -> (A -> A) -> A -> A

zero : numeral
zero = λ A f x. x

one : numeral
one = λ A f x. f x

two : numeral
two = λ A f x. f (f x)

three : numeral
three = λ A f x. f (f (f x))

plus : numeral -> numeral -> numeral
plus =
  λ m n.
    λ A f x.
      m A f (n A f x)

Check plus
-- ((A_235 : Type) → (A_235 → A_235) → A_235 → A_235) → ((A_272 : Type)
--     → (A_272 → A_272) → A_272 → A_272) → (A : Type) → (A → A) → A → A

Eval plus three two Nat Succ Zero
-- Succ (Succ (Succ (Succ (Succ Zero))))
-- : Nat
~~~

`Nat` addition implemented with dependent eliminator:

~~~
Nat  : Type
Zero : Nat
Succ : Nat -> Nat

nat-elim : ∀ (m : Nat -> Type)
        -> m Zero
        -> (∀ (l : Nat) -> m l -> m (Succ l))
        -> (k : Nat)
        -> m k

Check nat-elim

e-plus : Nat -> Nat -> Nat
e-plus = nat-elim
  (λ (_ : Nat). Nat -> Nat)
  (λ (n : Nat). n)
  (λ (k : Nat) (rec : (Nat -> Nat)) (n : Nat). Succ (rec n))

Eval e-plus

~~~
