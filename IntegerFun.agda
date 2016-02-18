module IntegerFun where

  open import IntegerFun.String
  open import IntegerFun.Sign
  open import IntegerFun.Natural

  data ℤ : Set where
    -[1+_] : (n : ℕ) → ℤ
    +_ : (n : ℕ) → ℤ

  ∣_∣ : ℤ → ℕ
  ∣ + n ∣ = n
  ∣ -[1+ n ] ∣ = ℕ.suc n

  -_ : ℤ → ℤ
  - (+ ℕ.suc n) = -[1+ n ]
  - (+ ℕ.zero) = + ℕ.zero
  - -[1+ n ] = + ℕ.suc n

  _sub_ : ℕ → ℕ → ℤ
  m sub ℕ.zero = + m
  ℕ.zero sub ℕ.suc n = -[1+ n ]
  ℕ.suc m sub ℕ.suc n = m sub n

  sign : ℤ → Sign
  sign (+ _) = Sign.positive
  sign -[1+ _ ] = Sign.negative

  render : ℤ → String
  render i = renderSign (sign i) ++ show ∣ i ∣
