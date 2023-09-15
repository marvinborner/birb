{-# LANGUAGE LambdaCase #-}

module Term
  ( Term(..)
  , fromBLC
  , nf
  ) where

import           Data.IORef                     ( IORef
                                                , modifyIORef
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )
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

shift :: Int -> Term -> Term
shift i (Idx j) | i <= j    = Idx $ j + 1
                | otherwise = Idx j
shift i (App a b) = App (shift i a) (shift i b)
shift i (Abs a  ) = Abs (shift (i + 1) a)

subst :: Int -> Term -> Term -> Term
subst i (Idx j) c | i == j    = c
                  | j > i     = Idx $ j - 1
                  | otherwise = Idx j
subst i (App a b) c = App (subst i a c) (subst i b c)
subst i (Abs a  ) c = Abs (subst (i + 1) a (shift 0 c))

nf :: Term -> IO Term
nf o = do -- TODO: pointfree??
  -- i <- newIORef 1000
  i <- newIORef 100000000
  go i o
 where
  go :: IORef Integer -> Term -> IO Term
  go i t = do -- oracle
    readIORef i >>= \case
      -- 0    -> writeIORef i (-1) >> return (Idx 0)
      0 -> do
        putStrLn "💥 potential infinite loop, continue? [yn]"
        getLine >>= \case
          "y" -> writeIORef i (-2) >> re i t
          "n" -> writeIORef i (-1) >> return t
          _   -> go i t
      (-1) -> return t
      _    -> modifyIORef i (subtract 1) >> re i t

  re :: IORef Integer -> Term -> IO Term
  re i (App l r) = go i l >>= \case
    Abs t -> go i (subst 0 t r)
    t     -> App t <$> go i r
  re i (Abs t) = Abs <$> go i t
  re _ t       = pure t
