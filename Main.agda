module Main where
-- cd /usr/local/lib/agda/ffi
-- cabal install
-- agda -i . -i /usr/local/lib/agda/src -c Main.agda

  open import Data.Bool
  open import Data.Char
  open import Data.List
  open import Data.Maybe
  open import Data.Nat
  open import Data.Nat.Show
  open import Data.String
  open import Foreign.Haskell using (Unit)
  open import IO.Primitive
  open import Data.Bool.Base using (not)
  open import Data.Product
  open import Function

  open import IntegerFun as ℤ
    using (ℤ) renaming (_plus_ to _ℤplus_)

  postulate
    getLine : IO String

  {-# COMPILED getLine getLine #-}

  -- source: http://dafoster.net/articles/2014/02/17/agda-second-impressions/
  parseInt : String → Maybe ℕ
  parseInt s =
    then? (unwrap (parseDigits s)) (λ s' →
      just (digitsToℕ s'))
    where
      parseDigits : String → List (Maybe ℕ)
      parseDigits s = Data.List.map toDigit (Data.String.toList s)
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
          unwrap' (just xs) (just y ∷ ys) = unwrap' (just (Data.List._++_ xs Data.List.[ y ])) ys
          unwrap' (just xs) (nothing ∷ _) = nothing
          unwrap' (just xs) [] = just xs
          unwrap' nothing _ = nothing

      then? : {A : Set} → {B : Set} → Maybe A → (A → Maybe B) → Maybe B
      then? nothing _ = nothing
      then? (just r1) op2 = op2 r1

      digitsToℕ : List ℕ → ℕ
      digitsToℕ xs = digitsToℕ' (Data.List.reverse xs)
        where
          digitsToℕ' : List ℕ → ℕ
          digitsToℕ' []       = 0
          digitsToℕ' (x ∷ xs) = x + (10 * (digitsToℕ' xs))

  ℕ? : Maybe ℕ → ℕ
  ℕ? (just x) = x
  ℕ? nothing  = 0

  notSpaceChar : Char → Bool
  notSpaceChar ' ' = false
  notSpaceChar _ = true

-- source: https://stackoverflow.com/questions/11763639/agda-parse-a-string-with-numbers
  splitBy : ∀ {a} {A : Set a} → (A → Bool) → List A → List (List A)
  splitBy {A = A} p = uncurry′ _∷_ ∘ foldr step ([] , [])
    where
      step : A → List A × List (List A) → List A × List (List A)
      step x (cur , acc) with p x
      ... | true  = x ∷ cur , acc
      ... | false = [] , cur ∷ acc

  segregate : String → List String
  segregate string = Data.List.map fromList (splitBy notSpaceChar (toList string))

  parseNumbers : String → List (Maybe ℕ)
  parseNumbers s = Data.List.map parseInt (segregate s)

  main : IO Unit
  main =
    getLine >>= (λ s →
      return (Data.Nat.Show.show (ℕ? (parseInt s))) >>= (λ s' →
        putStrLn (toCostring s')))
