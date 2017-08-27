module Pieces where

import Data.Array.Unboxed
import GHC.Read
import Text.ParserCombinators.ReadPrec (readP_to_Prec, readPrec_to_P)
import qualified Data.Char
import Text.ParserCombinators.ReadP
import Atoms

mkReadPrec r = readP_to_Prec (\_ -> r)
mkReadP r = readPrec_to_P r 0

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
            return $ index pair c
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

-- attacks :: Piece -> [Vector]
-- attacks = map (uncurry Vector) . attacks' where
--   attacks' piece = case piece of
--                      Prime R -> rep4 (0,1)
--                      Prime B -> rep4 (1,1)
--                      Prime N -> rep4 (1,2) ++ rep4 (2,1)
--                      Hybrid p1 p2 -> attacks' (Prime p1) ++ attacks' (Prime p2)
--                      P -> [(1,1), (-1,1)]
--                      _ | piece `elem` [Prime G, K] -> rep4 (0,1) ++ rep4 (1,1)

-- land :: (Piece, Color) -> Item -> Maybe Item
-- land (actor, color) target =
--   case target of
--     Empty -> Just $ Piece color actor
--     Piece targetColor targetPiece ->
--       if color == targetColor then
--         case (actor, targetPiece) of
--           -- join
--           (Prime p1, Prime p2) -> Just $ Piece color (Hybrid p1 p2)
--           _ -> Nothing
--       else
--         -- capture
--         Just $ Piece color actor


data Position = Position { board :: Board
                         , turn :: Color
                           -- TODO
                         }

data MoveSign = Pass | Capture | Join
data SrcLoc = SrcLoc (Maybe Int) (Maybe Int)

-- data MoveDesc = MoveDesc { actor :: Piece Option
--                          , src :: Loc
--                          , dst :: SrcLoc
--                          , moveSignd :: MoveSign
--                          -- TODO: promotion, etc
--                          }
data MoveDesc = MoveDesc { actor :: Piece
                         , src :: Loc
                         , dst :: Loc
                         , moveSignd :: MoveSign
                         -- TODO: promotion, etc
                         }

tryMove :: Position -> MoveDesc -> Either String Position
tryMove (Position (Board brd) turn) (MoveDesc actor src dst _) = do
  let srcPiece = brd ! src
  leftItem <- bar srcPiece
  (basicLeaper, basicVector, range) <- foo
  undefined where
    vec@(Vector u v) = dst `sub` src
    u' = abs u
    v' = abs v
    su = signum u
    sv = signum v
    foo :: Either String (BasicLeaper, Vector, Int)
    foo = case (u, v) of
            (0, 0) -> Left "dst == src"
            (0, _) -> Right (Vizir, Vector 0 sv, v)
            (_, 0) -> Right (Vizir, Vector su 0, u)
            _ | u' == v' -> Right (Ferz, Vector su sv, 0)
            _ | u' * 2 == v' -> Right (Knight, Vector (2 * su) sv, v)
            _ | v' * 2 == u' -> Right (Knight, Vector su (2 * sv), u)
            _ -> Left "no such leaper"
    bar :: Item -> Either String Item
    bar srcItem = case srcItem of
                    Empty -> Left "empty square"
                    Piece col p | col == turn ->
                                    case p of
                                      Hybrid p1 p2 -> case actor of Prime a | a == p1 -> Right $ Piece turn $ Prime p2
                                                                    Prime a | a == p2 -> Right $ Piece turn $ Prime p1
                                                                    _ -> Left "wrong actor"
                                      _ | p == actor -> Right Empty
                    Piece _ _ -> Left "wrong turn"


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
    _ -> (readItem >>= (\item -> readFEN(r, f+1, (Loc f r, item):acc))) <++ readDigit state

-- reads FEN
instance Read Board where
  readPrec = mkReadPrec $ do
    ls <- readFEN (0, 0, [])
    return $ Board (listArray (Loc 0 0, Loc 7 7) (repeat Empty) // ls)

-- writes FEN
instance Show Board where
  show (Board arr) = showFEN 0 0 0
    where
      showFEN 7 empties 8 = showEmpties empties
      showFEN r empties 8 = showEmpties empties ++ "/" ++ showFEN (r+1) 0 0
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

emptyBoard = Board $ listArray (Loc 0 0, Loc 7 7) (repeat Empty)
initialBoard :: Board
initialBoard = read "rnbgkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBGKBNR"
