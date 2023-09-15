module Term
  ( Term(..)
  , fromBLC
  ) where

import           Utils

data Term = Abs Term | App Term Term | Idx Int
  deriving (Eq, Ord)

instance Show Term where
  showsPrec _ (Abs body) = showString "[" . shows body . showString "]"
  showsPrec _ (App lhs rhs) =
    showString "(" . shows lhs . showString " " . shows rhs . showString ")"
  showsPrec _ (Idx i) = shows i

fromBLC' :: String -> (Term, String)
fromBLC' inp = case inp of
  '0' : '0' : rst -> let (e, es) = fromBLC' rst in (Abs e, es)
  '0' : '1' : rst ->
    let (exp1, rst1) = fromBLC' rst
        (exp2, rst2) = fromBLC' rst1
    in  (App exp1 exp2, rst2)
  '1' : _ : rst -> binaryBruijn rst
  _             -> invalid
 where
  binaryBruijn rst =
    let idx = length (takeWhile (== '1') inp) - 1
    in  case rst of
          "" -> (Idx idx, "")
          _  -> (Idx idx, drop idx rst)

fromBLC :: String -> Term
fromBLC = fst . fromBLC'
