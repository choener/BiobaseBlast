
module Biobase.SubstMatrix.Hints where



-- | Provide recommendations for both, substitution matrix to use, and gap
-- cost.

recommendByQueryLength ∷ Int → (String, GapCost)
recommendByQueryLength k
  | k <  35 = ("PAM30"   , gc  -9 -1)
  | k <= 50 = ("PAM70"   , gc -10 -1)
  | k <= 85 = ("BLOSUM80", gc -10 -1)
  | k >  85 = ("BLOSUM62", gc -10 -1)
  where gc init cont = GapCost { gcInit = init, gcCont = cont }

