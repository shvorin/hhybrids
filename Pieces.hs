module Pieces where

import Data.Array.Unboxed
import GHC.Read
import Text.ParserCombinators.ReadPrec (readP_to_Prec, readPrec_to_P)
import qualified Data.Char
import Text.ParserCombinators.ReadP

data Loc = Loc Int Int deriving (Eq, Ord, Ix)

mkReadPrec r = readP_to_Prec (\_ -> r)
mkReadP r = readPrec_to_P r 0

instance Show Loc where
  show (Loc f r) = [files !! f] ++ show r where
    files = ".abcdefgh"

ranks_range = ('1','8')
files_range = ('a','h')

instance Read Loc where
  readPrec = mkReadPrec $ do
    f <- readFileOrRank files_range
    r <- readFileOrRank ranks_range
    return $ Loc f r
      where
        readFileOrRank pair = do
          c <- get
          if inRange pair c then
            return (index pair c + 1)
            else pfail

instance Show Piece where
  show K = "K"
  show P = "p"
  show (Hybrid p1 p2) = show p1 ++ show p2
  show (Prime p) = show p

instance Read Piece where
  readPrec = mkReadPrec $ getHybridable <++ getOther <++ return P where
    getPrime = mkReadP readPrec
    getHybridable = do
      p@(Prime p1) <- getPrime
      ((\(Prime p2) -> Hybrid p1 p2) `fmap` getPrime) <++ return p
    getOther = do
      c <- get
      case c of
        'K' -> return K
        'p' -> return P
        _ -> pfail

data Piece = K | P | Hybrid Prime Prime | Prime Prime deriving Eq
data Prime = R | B | N | G deriving (Eq, Show)
data Color = Black | White deriving (Eq, Show)
data Item = Piece Color Piece | Empty deriving Show

data Board = Board (Array Loc Item)

data Vector = Vector Int Int

add :: Loc -> Vector -> Loc
add (Loc x y) (Vector u v) = Loc (x+u) (y+v)

rot90 (x,y) = (-y, x)
rep4 v = take 4 $ rep v where
  rep v = v:rep (rot90 v)

attacks :: Piece -> [Vector]
attacks = map (uncurry Vector) . attacks' where
  attacks' piece = case piece of
                     Prime R -> rep4 (0,1)
                     Prime B -> rep4 (1,1)
                     Prime N -> rep4 (1,2) ++ rep4 (2,1)
                     Hybrid p1 p2 -> attacks' (Prime p1) ++ attacks' (Prime p2)
                     P -> [(1,1), (-1,1)]
                     _ | piece `elem` [Prime G, K] -> rep4 (0,1) ++ rep4 (1,1)

land :: (Piece, Color) -> Item -> Maybe Item
land (actor, color) target =
  case target of
    Empty -> Just $ Piece color actor
    Piece targetColor targetPiece ->
      if color == targetColor then
        case (actor, targetPiece) of
          -- join
          (Prime p1, Prime p2) -> Just $ Piece color (Hybrid p1 p2)
          _ -> Nothing
      else
        -- capture
        Just $ Piece color actor

iterMoves :: Piece -> Board -> Loc -> [Loc]
iterMoves piece (Board board) src =
  do
    v <- attacks piece
    let foo x =
          let
            y = add x v
          in
            if inRange (bounds board) y then
              case board ! y of
                Empty | isRanger piece -> y : foo y
                _ -> [y]
            else
              []
            
    x <- foo src
    return x

isRanger piece = case piece of
                  Prime p -> isRanger p
                  Hybrid p1 p2 -> isRanger p1 || isRanger p2
                  _ -> False
  where
    isRanger R = True
    isRanger B = True
    isRanger _ = False

readItem = between (char '(') (char ')') readHybrid <++ readSingle where
  readSingle = do
    c <- get
    piece <- do
      case Data.Char.toUpper c of
        'N' -> return $ Prime N
        'G' -> return $ Prime G
        'R' -> return $ Prime R
        'B' -> return $ Prime B
        'K' -> return K
        'P' -> return P
        _   -> pfail
    return $ Piece (if Data.Char.isLower c then White else Black) piece

  readHybrid = do
    (c1, p1) <- readPrime
    (c2, p2) <- readPrime
    if c1 == c2
      then return $ Piece c1 $ Hybrid p1 p2
      else pfail

  readPrime = do
    c <- get
    prime <- do
      case Data.Char.toUpper c of
        'N' -> return N
        'G' -> return G
        'R' -> return R
        'B' -> return B
        _   -> pfail
    return (if Data.Char.isLower c then White else Black, prime)
    
readDigit (r, f, acc) = do
  c <- get
  if inRange ranks_range c then
    let f' = index ranks_range c + 1 in
      readFEN (r, f + f', acc)
    else pfail

readFEN state@(r, f, acc) = do
  case (r, f) of
    (7, 8) -> return acc
    (_, 8) -> char '/' >> readFEN(r+1, 0, acc)
    _ -> (readItem >>= (\item -> readFEN(r, f+1, (Loc (f+1) (r+1), item):acc))) <++ readDigit state

-- reads FEN
instance Read Board where
  readPrec = mkReadPrec $ do
    ls <- readFEN (0, 0, [])
    return $ Board (listArray (Loc 1 1, Loc 8 8) (repeat Empty) // ls)

-- writes FEN
instance Show Board where
  show (Board arr) = showFEN 1 0 1
    where
      showFEN 8 empties 9 = showEmpties empties
      showFEN r empties 9 = showEmpties empties ++ "/" ++ showFEN (r+1) 0 1
      showFEN r empties f = case arr ! (Loc f r) of
            Empty -> showFEN r (empties+1) (f+1)
            Piece c piece -> showEmpties empties ++ showPiece c piece ++ showFEN r 0 (f+1)

      showPiece c piece = wrap $ fixCase $ show piece where
        fixCase = case c of White -> map Data.Char.toLower
                            Black -> map Data.Char.toUpper
        wrap = case piece of Hybrid _ _ -> \str -> "(" ++ str ++ ")"
                             _ -> id
      showEmpties 0 = ""
      showEmpties empties = show empties

emptyBoard = Board $ listArray (Loc 1 1, Loc 8 8) (repeat Empty)
initialBoard :: Board
initialBoard = read "rnbgkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBGKBNR"
