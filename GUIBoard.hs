module GUIBoard where

import Graphics.UI.WX
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.Draw
import System.FilePath (FilePath, (</>))
import Data.Array.Unboxed
import Atoms
import Pieces

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  dragPos <- varCreate pointNull
  selected <- varCreate Nothing
  currBoard <- varCreate initialBoard

  let onPaint1 dc viewArea = do
        drawRect dc (Rect (xsize-1) (xsize-1) (8*xsize+2) (8*xsize+2)) [ ]
        let drawLoc (Loc f r) = do
              drawRect dc (Rect (xsize*r) (xsize*f) xsize xsize) [ brush := brushSolid rgbCol ]
                where rgbCol = if (f + r) `mod` 2 == 0
                               then colorRGB 153 153 153 else colorRGB 243 243 243
        mapM_ drawLoc $ range (Loc 1 1, Loc 8 8)

        mapM_ (\r -> drawText dc (show (9-r)) (Point (xsize `div` 2) (xsize * r + xsize `div` 2)) [])
          [1..8]

        mapM_ (\f -> drawText dc [" abcdefgh " !! f]
                     (Point (xsize * f + xsize `div` 2) (xsize * 9 + xsize `div` 2)) [])
          [1..8]

  let onPaint dc viewArea = do
        onPaint1 dc viewArea
        sp <- varGet selected
        brd@(Board arr) <- varGet currBoard
        drawBoard dc brd sp
        pos <- varGet dragPos
        case sp of
         Nothing -> return ()
         Just (loc, rest, actor) -> do
           drawItemAt dc actor pos
           circle dc pos 20 [brush := brushSolid red]

  let onClick point = do
        brd <- varGet currBoard
        let sp = selectPiece point brd
        putStrLn $ "selected: " ++ show sp ++ ", at " ++ show point
        varSet selected sp
        varSet dragPos point

  let onUnclick point = do
        varSet selected Nothing

  let onDrag point = do
        varSet dragPos point
  
  f <- frame [ resizeable := False ]
  p <- panel f [on keyboard      := \k -> (putStrLn $ "key: " ++ show k)
               , on paint := onPaint
               , on click := onClick
               , on unclick := onUnclick
               , on drag := onDrag
--               , bgcolor := white
               ]
  t <- timer f [ interval   := 50
               , on command := repaint p
               ]
  set f [layout := boxed "foo" $ container p $ space (10*xsize) (10*xsize)
        ]
  return ()

data PieceImage p = None
                  | Single p
                  | Double p p deriving Show

instance Functor PieceImage where
  fmap f None = None
  fmap f (Single p1) = Single $ f p1
  fmap f (Double p1 p2) = Double (f p1) (f p2)

getBitmapFile :: Item -> PieceImage FilePath
getBitmapFile Empty = None
getBitmapFile (Piece c p) =
  case p of (Hybrid p1 p2) -> Double (mkPath $ primeName p1) (mkPath $ primeName p2)
            Prime p1  -> Single $ mkPath $ primeName p1
            K -> Single $ mkPath "king"
            P -> Single $ mkPath "pawn"
  where
    mkPath pName = "images" </> (pName ++ cName ++ ".gif")
    cName = case c of { White -> "w"; Black -> "b" }
    primeName p = case p of G -> "queen"
                            N -> "knight"
                            R -> "rook"
                            B -> "bishop"

getBitmap :: Item -> PieceImage (Bitmap ())
getBitmap = fmap bitmap . getBitmapFile

xsize = 96

data Placement = Center | Upper | Lower deriving Show

placePiece :: Placement -> Point -> Size -> Point
placePiece placement (Point x y) (Size w h) = Point x' y' where
  x' = x - w `div` 2
  y' = y - h `div` 2 + (xsize*offset) `div` 6
  offset = case placement of { Center -> 0 ; Upper -> -1 ; Lower -> 1}

drawItem :: DC () -> Item -> Maybe (Loc, Item, Item) -> Loc -> IO ()
drawItem dc item masked loc@(Loc f r) =
  drawItemAt dc item' (Point (f*xsize + xsize `div` 2) ((9-r)*xsize + xsize `div` 2)) where
    item' = case masked of
             Just (loc', rest, _) | loc == loc' -> rest
             _ -> item

drawItemAt :: DC () -> Item -> Point -> IO ()
drawItemAt dc item point =
  case getBitmap item of
   None -> return ()
   Single bm -> drawBitmapAt bm Center
   Double bm1 bm2 -> drawLower >> drawUpper
     where
       drawLower = drawBitmapAt bm1 Lower
       drawUpper = drawBitmapAt bm2 Upper
  where
    drawBitmapAt bm placement = do
      sz <- get bm size
      drawBitmap dc bm (placePiece placement point sz) True []

drawBoard :: DC () -> Board -> Maybe (Loc, Item, Item) -> IO ()
drawBoard dc (Board arr) masked =
  mapM_ (\(loc, item) -> drawItem dc item masked loc) $ assocs arr

selectLoc :: Point -> Maybe Loc
selectLoc (Point x y) =
  if inRange (1,8) f && inRange (1,8) r then Just loc else Nothing
  where
    f = x `div` xsize
    r = y `div` xsize
    loc = Loc f (9-r)

selectPiece :: Point -> Board -> Maybe (Loc, Item, Item)
selectPiece point@(Point x y) (Board arr) = do
  loc <- selectLoc point
  let item = arr ! loc
  case item of
   Empty -> Nothing
   Piece c (Hybrid p1 p2) -> Just (loc, rest, actor)
     where
       (rest, actor) = case ((y*3) `div` xsize) `mod` 3 of
         0 {- Upper -}  -> (Piece c (Prime p1), Piece c (Prime p2))
         1 {- Center -} -> (Empty, item)
         2 {- Lower -}  -> (Piece c (Prime p2), Piece c (Prime p1))
   Piece c _ -> Just (loc, Empty, item)
