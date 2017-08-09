module GUIBoard where

import Graphics.UI.WX
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.Draw
import System.FilePath (FilePath, (</>))
import Data.Array.Unboxed
import Pieces

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  selected <- varCreate Nothing
  currBoard <- varCreate initialBoard

  let onPaint1 dc viewArea = do
        drawRect dc (Rect (xsize-1) (xsize-1) (8*xsize+2) (8*xsize+2)) [ ]
        let drawField col row = do
              drawRect dc (Rect (xsize*row) (xsize*col) xsize xsize) [ brush := brushSolid rgbCol ]
                where rgbCol = if (col + row) `mod` 2 == 0
                               then colorRGB 153 153 153 else colorRGB 243 243 243
        sequence_ [drawField col row | col <- [1..8], row <- [1..8]]

        sequence_ [drawText dc (show (9-col))
                   (Point (xsize `div` 2) (xsize * col + xsize `div` 2)) [] | col <- [1..8]]
        
        sequence_ [drawText dc [" abcdefgh " !! row]
                   (Point (xsize * row + xsize `div` 2) (xsize * 9 + xsize `div` 2)) [] | row <- [1..8]]

  let onPaint dc viewArea = do
        onPaint1 dc viewArea
        brd@(Board arr) <- varGet currBoard
        drawBoard dc brd
        sp <- varGet selected
        case sp of
          Just (field, placement, pos) -> do
            let item = arr ! field
            drawItemAt dc item pos False
            circle dc pos 20 [brush := brushSolid red]
          Nothing -> return ()

  let onClick point = do
        let sp = selectPiece point
        putStrLn $ "selected: " ++ show sp ++ ", at " ++ show point
        varSet selected $ sp

  let onUnclick point = do
        putStrLn $ "drop to " ++ (show $ selectPiece point)
        varSet selected Nothing

  let onDrag point = do
        sp <- varGet selected
        case sp of
          Nothing -> return ()
          Just (field, placement, _) -> varSet selected (Just (field, placement, point))
  
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

placePieceAligned :: Placement -> Point -> Size -> Point
placePieceAligned placement (Point x y) (Size w h) = Point x' y' where
  x' = x + (xsize - w) `div` 2
  y' = y + (xsize*offset - h*3) `div` 6
  offset = case placement of { Center -> 3 ; Upper -> 2 ; Lower -> 4}

placePiece :: Point -> Size -> Point
placePiece (Point x y) (Size w h) = Point x' y' where
  x' = x - w `div` 2
  y' = y - h `div` 2

drawItem :: DC () -> Item -> Field -> IO ()
drawItem dc item (Field f r) = drawItemAt dc item (Point (f*xsize) ((9-r)*xsize)) True

drawItemAt :: DC () -> Item -> Point -> Bool -> IO ()
drawItemAt dc item point align =
  case getBitmap item of
   None -> return ()
   Single bm -> drawBitmapAt bm Center
   Double bm1 bm2 -> drawBitmapAt bm1 Lower >> drawBitmapAt bm2 Upper
  where
    drawBitmapAt bm placement = do
      sz <- get bm size
      let point' = if align then placePieceAligned placement point sz
                   else placePiece point sz
      drawBitmap dc bm point' True []

drawBoard :: DC () -> Board -> IO ()
drawBoard dc (Board arr) =
  mapM_ (\(field, item) -> drawItem dc item field) $ assocs arr

type Selected = (Field, Placement, Point)
selectPiece :: Point -> Maybe Selected
selectPiece point@(Point x y) =
  if inRange (1,8) f && inRange (1,8) r then
    Just (Field f (9-r), placement, point)
  else Nothing
  where
    f = x `div` xsize
    r = y `div` xsize
    r3 = (y*3) `div` xsize
    placement = case r3 `mod` 3 of
                 0 -> Upper
                 1 -> Center
                 2 -> Lower
