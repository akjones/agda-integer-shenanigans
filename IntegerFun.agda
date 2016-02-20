module IntegerFun where

  open import IntegerFun.String
  open import IntegerFun.Sign renaming (show to Signshow)
  open import IntegerFun.Natural as ℕ
    using (ℕ) renaming (_plus_ to _ℕplus_; _minus_ to _ℕminus_; show to ℕshow)

  data ℤ : Set where
    -[1+_] : (n : ℕ) → ℤ
    +_ : (n : ℕ) → ℤ

  ∣_∣ : ℤ → ℕ
  ∣ + n ∣ = n
  ∣ -[1+ n ] ∣ = ℕ.suc n

  -_ : ℤ → ℤ
  - (+ ℕ.suc n) = -[1+ n ]
  - (+ ℕ.zero) = + (ℕ.zero)
  - -[1+ n ] = + (ℕ.suc n)

  _minus_ : ℕ → ℕ → ℤ
  m minus ℕ.zero = + m
  ℕ.zero minus ℕ.suc n = -[1+ n ]
  ℕ.suc m minus ℕ.suc n = m minus n

  _plus_ : ℤ → ℤ → ℤ
  + m plus + n = + (m ℕplus n)
  + m plus -[1+ n ] = m minus ℕ.suc n
  -[1+ m ] plus + n = n minus ℕ.suc m
  -[1+ m ] plus -[1+ n ] = -[1+ ℕ.suc (m ℕplus n) ]

  sign : ℤ → Sign
  sign (+ _) = Sign.positive
  sign -[1+ _ ] = Sign.negative

  show : ℤ → String
  show i = Signshow (sign i) ++ ℕshow ∣ i ∣
