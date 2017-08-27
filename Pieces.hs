module Pieces where

import Control.Monad
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
      ((\(Prime p2) -> hybridSorted p1 p2) `fmap` getPrime) <++ return p
    getOther = do
      c <- get
      case c of
        'K' -> return K
        'p' -> return P
        _ -> pfail

data Piece = K | P | Hybrid Prime Prime | Prime Prime deriving Eq
data Prime = R | B | N | G deriving (Eq, Ord, Show)
data Color = Black | White deriving (Eq, Show)
data Item = Piece Color Piece | Empty deriving (Show, Eq)

hybridSorted p1 p2 | p1 > p2 = Hybrid p2 p1
                   | otherwise = Hybrid p1 p2

inv Black = White
inv White = Black

data Board = Board (Array Loc Item)

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

leave :: MonadPlus m => Item -> Piece -> m Item
leave Empty _ = mzero
leave (Piece col p) actor =
  case p of
    Hybrid p1 p2 | actor == Prime p1 -> return $ Piece col $ Prime p2
                 | actor == Prime p2 -> return $ Piece col $ Prime p1
    _ | actor == p                   -> return Empty
      | otherwise                    -> mzero

occupy :: MonadPlus m => Color -> Item -> Piece -> m Item
occupy turn Empty actor                                     = return $ Piece turn actor
occupy turn (Piece col _) actor               | col /= turn = return $ Piece turn actor
occupy turn (Piece col (Prime p0)) (Prime p1) | turn == col = return $ Piece turn $ hybridSorted p0 p1
occupy _ _ _                                                = mzero

liftBool :: MonadPlus m => Bool -> m ()
liftBool True = return ()
liftBool False = mzero

xor :: Bool -> Bool -> Bool
xor x y = x /= y

makeMove :: MonadPlus m => Position -> MoveDesc -> m Position
makeMove (Position (Board brd) turn) (MoveDesc actor src@(Loc _ ySrc) dst _) = do
  leftItem <- leave srcItem actor
  occItem <- occupy turn dstItem actor
  pmove@(basicLeaper, basicVector, range) <- parseMovement vec
  _ <- liftBool $ matchPiece pmove
  _ <- liftBool $ all checkRider [1..range-1]
  return $ Position (Board (brd // [(src, leftItem), (dst, occItem)])) (inv turn)
  where
    srcItem = brd ! src
    dstItem = brd ! dst
    vec@(Vector _ yVec) = dst `sub` src
    checkRider k = (brd ! (src `add` (k `mult` vec))) == Empty

    matchPiece :: (BasicLeaper, Vector, Int) -> Bool
    matchPiece (basicLeaper, basicVector, range) =
      case actor of
        Hybrid p1 p2 -> matchPrime p1 || matchPrime p2
        Prime p1     -> matchPrime p1
        K            -> matchPrime G
        P            -> matchPawn
      where
        matchPrime R = basicLeaper == Vizir
        matchPrime B = basicLeaper == Ferz
        matchPrime N = range == 1 && basicLeaper == Knight
        matchPrime G = range == 1 && basicLeaper `elem` [Vizir, Ferz]

        matchPawn = pawnIsForward && pawnMatchLeaper && case range of
                                                          1 -> True
                                                          2 -> pawnIsAtSecond

        pawnIsForward = (yVec > 0) `xor` (turn == White)
        pawnIsAtSecond = case turn of White -> ySrc == 1
                                      Black -> ySrc == 6
        pawnMatchLeaper = case basicLeaper of Ferz -> isAttack
                                              Vizir -> not isAttack
                                              _ -> False

    isAttack = case dstItem of Piece col _ | col /= turn -> True
                               _ -> False

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
      then return $ Piece c1 $ hybridSorted p1 p2
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

initialPosition = Position initialBoard White

