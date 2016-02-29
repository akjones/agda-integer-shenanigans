module IntegerFun.Sign where

  open import Data.String using (String)

  data Sign : Set where
    positive : Sign
    negative : Sign

  show : Sign → String
  show positive = ""
  show negative = "-"

