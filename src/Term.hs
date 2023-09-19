{-# LANGUAGE LambdaCase #-}

module Term
  ( Term(..)
  , fromBLC
  , nf
  ) where

import           Control.Concurrent.MVar
import           Data.IORef                     ( IORef
                                                , modifyIORef
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Data.List                      ( elemIndex )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
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

-- RKNL abstract machine, call-by-need reducer
-- written for my bruijn programming language

type Store = Map Int Box
type Stack = [Redex]

newtype NameGen = NameGen Int
data BoxValue = Todo Redex | Done Redex | Empty
newtype Box = Box (MVar BoxValue)
data Ridx = Num Int | Hole
data Redex = Rabs Int Redex | Rapp Redex Redex | Ridx Ridx | Rclosure Redex Store | Rcache Box Redex
data Conf = Econf NameGen Redex Store Stack | Cconf NameGen Stack Redex | End

nextName :: NameGen -> (Int, NameGen)
nextName (NameGen x) = (x, NameGen $ x + 1)

toRedex :: Term -> Redex
toRedex = convertWorker (NameGen 1) []
 where
  convertWorker g ns (Abs e) =
    let (v, g') = nextName g
        t       = convertWorker g' (v : ns) e
    in  Rabs v t
  convertWorker g ns (App l r) =
    let lhs = convertWorker g ns l
        rhs = convertWorker g ns r
    in  Rapp lhs rhs
  convertWorker _ ns (Idx i) =
    Ridx $ Num (if i < 0 || i >= length ns then i else ns !! i)
  convertWorker _ _ _ = invalid

fromRedex :: Redex -> Term
fromRedex = convertWorker []
 where
  convertWorker es (Rabs n e) = Abs $ convertWorker (n : es) e
  convertWorker es (Rapp l r) =
    let lhs = convertWorker es l
        rhs = convertWorker es r
    in  App lhs rhs
  convertWorker es (Ridx (Num n)) = Idx $ fromMaybe n (elemIndex n es)
  convertWorker _  _              = invalid

transition :: Conf -> IO Conf
transition (Econf g (Rapp u v) e s) =
  pure $ Econf g u e (Rapp (Ridx Hole) (Rclosure v e) : s)
transition (Econf g (Rabs x t) e s) = do
  box <- newMVar Empty
  pure $ Cconf g s (Rcache (Box box) (Rclosure (Rabs x t) e))
transition (Econf g (Ridx (Num x)) e s) = do
  def <- newMVar $ Done $ Ridx $ Num x
  let b@(Box m) = Map.findWithDefault (Box def) x e
  rd <- readMVar m
  case rd of
    Todo (Rclosure v e') -> pure $ Econf g v e' (Rcache b (Ridx Hole) : s)
    Done t               -> pure $ Cconf g s t
    _                    -> invalid
transition (Cconf g ((Rcache (Box m) (Ridx Hole)) : s) t) = do
  modifyMVar_ m (\_ -> pure $ Done t)
  pure $ Cconf g s t
transition (Cconf g ((Rapp (Ridx Hole) ve) : s) (Rcache _ (Rclosure (Rabs x t) e)))
  = do
    box <- newMVar (Todo ve)
    pure $ Econf g t (Map.insert x (Box box) e) s
transition (Cconf g s (Rcache (Box m) (Rclosure (Rabs x t) e))) = do
  rd <- readMVar m
  case rd of
    Done v -> pure $ Cconf g s v
    Empty  -> do
      let (x1, g') = nextName g
      box <- newMVar $ Done $ Ridx $ Num x1
      pure $ Econf g'
                   t
                   (Map.insert x (Box box) e)
                   (Rabs x1 (Ridx Hole) : Rcache (Box m) (Ridx Hole) : s)
    Todo _ -> invalid
transition (Cconf g ((Rapp (Ridx Hole) (Rclosure v e)) : s) t) =
  pure $ Econf g v e (Rapp t (Ridx Hole) : s)
transition (Cconf g ((Rapp t (Ridx Hole)) : s) v) = pure $ Cconf g s (Rapp t v)
transition (Cconf g ((Rabs x1 (Ridx Hole)) : s) v) =
  pure $ Cconf g s (Rabs x1 v)
transition (Cconf _ [] _) = pure End
transition _              = invalid

forEachState :: Conf -> (Conf -> IO Conf) -> IO Conf
forEachState conf trans = trans conf >>= \case
  End  -> pure conf
  next -> forEachState next trans

-- TODO: NameGen is arbitrary to not conflict with toRedex
loadTerm :: Redex -> Conf
loadTerm t = Econf (NameGen 1000000) t Map.empty []

nf :: Term -> IO Term
nf e = do
  let redex = toRedex e
  forEachState (loadTerm redex) transition >>= \case
    Cconf _ [] v -> pure $ fromRedex v
    _            -> invalid
