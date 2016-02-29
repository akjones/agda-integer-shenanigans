module IntegerFun.Natural where

  open import Data.Nat public
  open import Data.Nat.Show public
  open import IntegerFun.List as List
  open import Data.Char
  open import Data.Maybe

  _plus_ : ℕ → ℕ → ℕ
  zero plus n = n
  (suc n) plus m = suc (n plus m)

  _minus_ : ℕ → ℕ → ℕ
  n minus zero = n
  zero minus n = n
  (suc n) minus (suc m) = n minus m

  -- inspired by http://dafoster.net/articles/2014/02/17/agda-second-impressions/
  parseNat : List Char → Maybe ℕ
  parseNat s =
    then? (unwrap (parseDigits s)) (λ s' →
      just (digitsToℕ s'))
    where
      parseDigits : List Char → List (Maybe ℕ)
      parseDigits s = List.map toDigit s
        where
          toDigit : Char → Maybe ℕ
          toDigit '0' = just 0
          toDigit '1' = just 1
          toDigit '2' = just 2
          toDigit '3' = just 3
          toDigit '4' = just 4
          toDigit '5' = just 5
          toDigit '6' = just 6
          toDigit '7' = just 7
          toDigit '8' = just 8
          toDigit '9' = just 9
          toDigit _   = nothing

      unwrap : List (Maybe ℕ) → Maybe (List ℕ)
      unwrap xs = unwrap' (just []) xs
        where
          unwrap' : Maybe (List ℕ) → List (Maybe ℕ) → Maybe (List ℕ)
          unwrap' (just xs) (just y ∷ ys) = unwrap' (just (List._++_ xs List.[ y ])) ys
          unwrap' (just xs) (nothing ∷ _) = nothing
          unwrap' (just xs) [] = just xs
          unwrap' nothing _ = nothing

      then? : {A : Set} → {B : Set} → Maybe A → (A → Maybe B) → Maybe B
      then? nothing _ = nothing
      then? (just r1) op2 = op2 r1

      digitsToℕ : List ℕ → ℕ
      digitsToℕ xs = digitsToℕ' (List.reverse xs)
        where
          digitsToℕ' : List ℕ → ℕ
          digitsToℕ' []       = 0
          digitsToℕ' (x ∷ xs) = x + (10 * (digitsToℕ' xs))
