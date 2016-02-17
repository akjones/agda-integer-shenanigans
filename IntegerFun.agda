module IntegerFun where
  data ℕ : Set where
    zero : ℕ
    suc : ℕ → ℕ

  _plus_ : ℕ → ℕ → ℕ
  zero plus n = n
  (suc n) plus m = suc (n plus m)

  _minus_ : ℕ → ℕ → ℕ
  n minus zero = n
  zero minus n = n
  (suc n) minus (suc m) = n minus m

  data ℤ : Set where
    -[1+_] : (n : ℕ) → ℤ
    +_ : (n : ℕ) → ℤ

  -_ : ℤ → ℤ
  - (+ ℕ.suc n) = -[1+ n ]
  - (+ ℕ.zero)  = + ℕ.zero
  - -[1+ n ]    = + ℕ.suc n

  _sub_ : ℕ → ℕ → ℤ
  m sub ℕ.zero = + m
  ℕ.zero sub ℕ.suc n = -[1+ n ]
  ℕ.suc m sub ℕ.suc n = m sub n

{-# BUILTIN NATURAL ℕ #-}
