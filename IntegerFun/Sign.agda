module IntegerFun.Sign where

  open import IntegerFun.String

  data Sign : Set where
    positive : Sign
    negative : Sign

  renderSign : Sign → String
  renderSign positive = ""
  renderSign negative = "-"

