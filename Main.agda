module Main where
-- cd /usr/local/lib/agda/ffi
-- cabal install
-- agda -i . -i /usr/local/lib/agda/src -c Main.agda

  open import Data.Bool
  open import Data.Char
  open import Data.Maybe
  open import Data.String
  open import Foreign.Haskell using (Unit)
  open import IO.Primitive

  open import IntegerFun.List as List
  open import IntegerFun.Integer as ℤ
    using (ℤ) renaming (_plus_ to _ℤplus_)

  postulate
    getLine : IO String

  {-# COMPILED getLine getLine #-}

  segregate : String → List String
  segregate string = List.map fromList (splitBy notSpaceChar (toList string))
    where
      notSpaceChar : Char → Bool
      notSpaceChar ' ' = false
      notSpaceChar _ = true

  parseNumbers : String → List ℤ
  parseNumbers s = List.map ℤ?(List.map ℤ.parseInt (segregate s))
    where
      ℤ? : Maybe ℤ → ℤ
      ℤ? (just x) = x
      ℤ? nothing  = ℤ.+ 0

  calculate : List ℤ → ℤ
  calculate zs = List.foldl _ℤplus_ (ℤ.+ 0) zs

  main : IO Unit
  main =
    putStrLn (toCostring "Enter the integers you'd like to add (separated by a space):") >>= (λ _ →
      getLine >>= (λ s →
        return (ℤ.show(calculate(parseNumbers s))) >>= (λ s' →
          putStrLn (toCostring s'))))
