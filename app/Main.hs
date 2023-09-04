module Main
  ( main
  ) where

import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Debug.Trace
import           System.Environment             ( getArgs )

data Term = Abs Term | App Term Term | Idx Int
  deriving (Eq, Ord)

type Birb = Char

invalid :: a
invalid = error "invalid program state"

instance Read Term where
  readsPrec _ s
    | c : s' <- s, isDigit c
    = [(Idx (digitToInt c), s')]
    | '!' : s' <- s, [(t, s'')] <- reads s'
    = [(Abs t, s'')]
    | '@' : s' <- s, [(t, s'')] <- reads s', [(u, s''')] <- reads s''
    = [(App t u, s''')]
    | otherwise
    = invalid

instance Show Term where
  showsPrec _ (Abs body   ) = showString "\\" . shows body
  showsPrec _ (App lhs rhs) = showString "@" . shows lhs . shows rhs
  showsPrec _ (Idx i      ) = shows i

birbify :: Term -> Map Term Birb -> String
birbify t m | t `Map.member` m = [Map.findWithDefault invalid t m]
            | (Abs a) <- t     = "\\" ++ birbify a m
            | (App l r) <- t   = "@" ++ birbify l m ++ birbify r m
            | (Idx i) <- t     = show i

termify :: String -> Map Birb Term -> Term
termify [c     ] m = Map.findWithDefault invalid c m
termify (c : cs) m = App (termify [c] m) (termify cs m)
termify _        _ = invalid

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

nf :: Term -> Term
nf (App l r) = case nf l of
  Abs t -> nf (subst 0 t r)
  t     -> App t (nf r)
nf (Abs t) = Abs (nf t)
nf t       = t

fromBirbs :: String -> Term
fromBirbs s =
  let birbsies = Map.fromList $ second read <$> trace (show birbs) birbs
      filtered = filter (`Map.member` birbsies) s
      term     = termify filtered birbsies
  in  nf term

fromTerm :: Term -> String
fromTerm t =
  let flipped  = (\(a, b) -> (b, a)) <$> birbs
      termsies = Map.fromList $ first read <$> trace (show flipped) flipped
  in  birbify t termsies

main :: IO ()
main = do
  args <- getArgs
  file <- readFile (head args)
  let reduced = fromBirbs file
  print reduced
  let duced = fromTerm reduced
  print duced
  return ()

birbs :: [(Birb, String)]
birbs =
  [ ('\x1FAB6', "!!!@@201")
  , ('\x1F426', "!0")
  , ('\x1F54A', "!!!!@@32@10")
  , ('\x1F424', "!!@0@@110")
  , ('\x1F425', "!!!@0@12")
  -- , ('\x1F423', "!@00!@00") --
  , ('\x1F989', "!!@0@10")
  , ('\x1F414', "!@00")
  , ('\x1F986', "!!1")
  , ('\x1F9A4', "!!@01")
  -- , ('\x1F9A9', "!!@1@00!@1@00") --
  , ('\x1F9A2', "!!!@@20@10")
  , ('\x1FABD', "!!!!@@3@20@10")
  , ('\x1F983', "!!@@100")
  , ('\x1F413', "!!!@@012")
  , ('\x1F99A', "!!!@1@20")
  , ('\x1F99C', "!!!@2@10")
  , ('\x1F985', "!!!!!@@43@@210")
  -- , ('\x1F427', "!0!!!@@20@10!!1") --
  ]
