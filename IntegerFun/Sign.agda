module IntegerFun.Sign where

  open import IntegerFun.String

  data Sign : Set where
    positive : Sign
    negative : Sign

  show : Sign → String
  show positive = ""
  show negative = "-"

