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
import           System.Environment             ( getArgs )

data Term = Abs Term | App Term Term | Idx Int
  deriving (Eq, Ord)

type Birb = Char

invalid :: a
invalid = error "invalid program state"

instance Show Term where
  showsPrec _ (Abs body) = showString "[" . shows body . showString "]"
  showsPrec _ (App lhs rhs) =
    showString "(" . shows lhs . showString " " . shows rhs . showString ")"
  showsPrec _ (Idx i) = shows i

birbify :: Term -> Map Term Birb -> String
birbify t m | t `Map.member` m = [Map.findWithDefault invalid t m]
            | (Abs a) <- t     = "[" ++ birbify a m ++ "]"
            | (App l r) <- t   = "(" ++ birbify l m ++ " " ++ birbify r m ++ ")"
            | (Idx i) <- t     = show i

termify :: String -> Map Birb Term -> Term
termify s m = foldlr (odd $ length lst) lst
 where
  go [c     ] = [Map.findWithDefault invalid c m]
  go (c : cs) = go [c] ++ go cs
  go _        = invalid
  lst = go s

  -- TODO: Rewrite so last/init aren't needed
  foldlr :: Bool -> [Term] -> Term
  foldlr _     [x]      = x
  foldlr _     [x, y]   = App x y
  foldlr True  xs       = let t = foldlr False (init xs) in App t (last xs)
  foldlr False (x : xs) = let t = foldlr True xs in App x t
  foldlr _     _        = invalid

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
  let birbsies = Map.fromList $ second parse <$> birbs
      filtered = filter (`Map.member` birbsies) s
      term     = termify filtered birbsies
  in  nf term

fromTerm :: Term -> String
fromTerm t =
  let flipped  = (\(a, b) -> (b, a)) <$> birbs
      termsies = Map.fromList $ first parse <$> flipped
  in  birbify t termsies

main :: IO ()
main = do
  args <- getArgs
  file <- readFile (head args)
  let reduced = fromBirbs file
  let duced   = fromTerm reduced
  putStrLn duced
  return ()

-- this isn't really relevant but I'm too lazy to type the terms manually
parse :: String -> Term
parse = fst . go
 where
  go ('!' : cs) = let (t, cs') = go cs in (Abs t, cs')
  go ('@' : cs) =
    let (l, cs' ) = go cs
        (r, cs'') = go cs'
    in  (App l r, cs'')
  go (i : cs) | isDigit i = (Idx $ digitToInt i, cs)
  go _                    = invalid

birbs :: [(Birb, String)]
birbs =
  [ ('\x1F426', "!0") -- bird
  , ('\x1F54A', "!!!!@@32@10") -- dove
  , ('\x1F424', "!!@0@@110") -- chick
  , ('\x1F425', "!!1") -- front chick
  , ('\x1F423', "!!!@0@21") -- hatching chick
  , ('\x1F989', "!!@0@10") -- owl
  , ('\x1F986', "!!@0@12") -- duck
  , ('\x1F9A4', "!!@01") -- dodo
  , ('\x1F9A9', "!!!@@201") -- flamingo
  , ('\x1F9A2', "!!!@@20@10") -- swan
  , ('\x1FABD', "!!!!@@3@20@10") -- wing
  , ('\x1F99A', "!!!@1@20") -- peacock
  , ('\x1F99C', "!@00") -- parrot
  , ('\x1F985', "!!!!!@@43@@210") -- eagle
  , ('\x1F427', "!!!@2@10") -- penguin
-- , ('\x1FAB6', "") -- feather
-- , ('\x1F413', "") -- rooster
-- , ('\x1F414', "") -- chicken
-- , ('\x1F983', "") -- turkey
  ]
