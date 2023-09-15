-- This code partly uses algorithms created by John Tromp.
-- Since they did not license their code (afaik), I assume it's okay to reuse it.

module Transpile
  ( transpileSKI
  , transpileBirb
  ) where

import           Term
import           Utils

data SKI = S | K | I | AppSKI SKI SKI | IdxSKI Int
  deriving (Eq, Ord)

data Birb = Swan | Kool | Bird | Quacky
  deriving (Eq, Ord, Show)

instance Show SKI where
  showsPrec _ S = showString "s"
  showsPrec _ K = showString "k"
  showsPrec _ I = showString "i"
  showsPrec _ (AppSKI x y) =
    showString "(" . shows x . showString " " . shows y . showString ")"
  showsPrec _ (IdxSKI i) = shows i

drip :: SKI -> SKI
drip i@(AppSKI (AppSKI S K) _) = i
drip (  IdxSKI 0             ) = invalid
drip (  IdxSKI i             ) = IdxSKI (i - 1)
drip (  AppSKI x y           ) = AppSKI (drip x) (drip y)
drip x                         = x

abstract :: SKI -> SKI
abstract (AppSKI sk@(AppSKI S K) _) = sk
abstract e = if freeIn (== 0) e then occabstract e else AppSKI K (drip e) where
  freeIn _  (AppSKI (AppSKI S K) _) = False
  freeIn fv (AppSKI x            y) = freeIn fv x || freeIn fv y
  freeIn fv (IdxSKI i             ) = fv i
  freeIn _  _                       = False
  isConst = not . freeIn (const True)
  occabstract (IdxSKI 0) = I
  occabstract (AppSKI m (IdxSKI 0)) | not (freeIn (== 0) m) = drip m
  occabstract (AppSKI (AppSKI l m) l') | l == l' -- && freeIn (==0) e1
                                                 =
    occabstract (AppSKI (AppSKI (AppSKI (AppSKI S S) K) l) m)
  occabstract (AppSKI m (AppSKI n l)) | isConst m && isConst n =
    occabstract (AppSKI (AppSKI (AppSKI S (abstract m)) n) l)
  occabstract (AppSKI (AppSKI m n) l) | isConst m && isConst l =
    occabstract (AppSKI (AppSKI (AppSKI S m) (abstract l)) n)
  occabstract (AppSKI (AppSKI m l) (AppSKI n l'))
    | l == l' && isConst m && isConst n = occabstract
      (AppSKI (AppSKI (AppSKI S m) n) l)
  occabstract (AppSKI e1 e2) = AppSKI (AppSKI S (abstract e1)) (abstract e2)
  occabstract _              = invalid

transpileSKI :: Term -> SKI
transpileSKI (Idx i  ) = IdxSKI i
transpileSKI (App m n) = AppSKI (transpileSKI m) (transpileSKI n)
transpileSKI (Abs m  ) = abstract (transpileSKI m)

fromSKI :: SKI -> Birb
fromSKI S = Swan
fromSKI K = Kool
fromSKI I = Bird
fromSKI _ = invalid

transpileBirb :: SKI -> [Birb]
transpileBirb (AppSKI a b) = case [a, b] of
  [AppSKI x l, AppSKI y r] -> invalid -- TODO
  [AppSKI x l, r] -> [Bird, Bird] ++ transpileBirb x ++ [fromSKI a, fromSKI b]
  [l, AppSKI r x] ->
    [Bird, Quacky] ++ transpileBirb x ++ [fromSKI r, fromSKI l]
  [l, r] -> [fromSKI l, fromSKI r]
transpileBirb (IdxSKI _) = invalid
transpileBirb s          = [fromSKI s]
