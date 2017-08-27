module Atoms where

import Data.Array.Unboxed

data Loc = Loc Int Int deriving (Eq, Ord, Ix)

data Vector = Vector Int Int

data Quadrant = Q0 | Q1 | Q2 | Q3

data BasicLeaper = Vizir {- (0, 1) -} | Ferz {- (1, 1) -} | Knight {- (1, 2), (2, 1) -}

add :: Loc -> Vector -> Loc
add (Loc x y) (Vector u v) = Loc (x+u) (y+v)

sub :: Loc -> Loc -> Vector
sub (Loc x y) (Loc x' y') = Vector (x-x') (y-y')

instance Show Loc where
  show (Loc f r) = toEnum (fromEnum 'a' + f - 1) : show r

rot90 (x,y) = (-y, x)
rep4 v = take 4 $ rep v where
  rep v = v:rep (rot90 v)
