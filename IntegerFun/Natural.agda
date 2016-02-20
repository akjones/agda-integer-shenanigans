module IntegerFun.Natural where

  open import Data.Nat public
  open import Data.Nat.Show public

  _plus_ : ℕ → ℕ → ℕ
  zero plus n = n
  (suc n) plus m = suc (n plus m)

  _minus_ : ℕ → ℕ → ℕ
  n minus zero = n
  zero minus n = n
  (suc n) minus (suc m) = n minus m
