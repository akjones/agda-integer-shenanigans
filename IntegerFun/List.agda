module IntegerFun.List where

  open import Data.Bool
  open import Data.Product
  open import Function

  open import Data.List using (List; reverse; map; foldl; foldr; _++_; [_]; _∷_; []) public

  -- source: https://stackoverflow.com/questions/11763639/agda-parse-a-string-with-numbers
  splitBy : ∀ {a} {A : Set a} → (A → Bool) → List A → List (List A)
  splitBy {A = A} p = uncurry′ _∷_ ∘ foldr step ([] , [])
    where
      step : A → List A × List (List A) → List A × List (List A)
      step x (cur , acc) with p x
      ... | true  = x ∷ cur , acc
      ... | false = [] , cur ∷ acc
