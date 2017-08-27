--{-# LANGUAGE FlexibleContexts #-}
module Atoms where

import Data.Array.Unboxed
import Control.Monad

data Loc = Loc Int Int deriving (Eq, Ord, Ix)

data Vector = Vector Int Int deriving Show

data Quadrant = Q0 | Q1 | Q2 | Q3

data BasicLeaper = Vizir {- (0, 1) -} | Ferz {- (1, 1) -} | Knight {- (1, 2), (2, 1) -}
  deriving (Eq, Show)

add :: Loc -> Vector -> Loc
add (Loc x y) (Vector u v) = Loc (x+u) (y+v)

sub :: Loc -> Loc -> Vector
sub (Loc x y) (Loc x' y') = Vector (x-x') (y-y')

mult :: Int -> Vector -> Vector
mult k (Vector x y) = Vector (k*x) (k*y)

instance Show Loc where
  show (Loc f r) = [toEnum (fromEnum 'a' + f), toEnum (fromEnum '1' + r)]

rot90 (x,y) = (-y, x)
rep4 v = take 4 $ rep v where
  rep v = v:rep (rot90 v)


-- FIXME: name
parseMovement :: MonadPlus m => Vector -> m (BasicLeaper, Vector, Int)
parseMovement (Vector u v) = case (u, v) of
  (0, 0) -> mzero -- "dst == src"
  (0, _) -> return (Vizir, Vector 0 sv, v')
  (_, 0) -> return (Vizir, Vector su 0, u')
  _ | u' == v' -> return (Ferz, Vector su sv, v')
  _ | u' * 2 == v' -> return (Knight, Vector (2 * su) sv, u')
  _ | v' * 2 == u' -> return (Knight, Vector su (2 * sv), v')
  _ -> mzero -- "no such leaper"
  where
    u' = abs u
    v' = abs v
    su = signum u
    sv = signum v
