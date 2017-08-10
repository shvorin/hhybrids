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
        brd@(Board arr) <- varGet currBoard
        drawBoard dc brd
        sp <- varGet selected
        case sp of
          Just (loc, placement, pos) -> do
            let item = arr ! loc
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
          Just (loc, placement, _) -> varSet selected (Just (loc, placement, point))
  
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

drawItem :: DC () -> Item -> Loc -> IO ()
drawItem dc item (Loc f r) = drawItemAt dc item (Point (f*xsize) ((9-r)*xsize)) True

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
  mapM_ (\(loc, item) -> drawItem dc item loc) $ assocs arr

type Selected = (Loc, Placement, Point)
selectPiece :: Point -> Maybe Selected
selectPiece point@(Point x y) =
  if inRange (1,8) f && inRange (1,8) r then
    Just (Loc f (9-r), placement, point)
  else Nothing
  where
    f = x `div` xsize
    r = y `div` xsize
    r3 = (y*3) `div` xsize
    placement = case r3 `mod` 3 of
                 0 -> Upper
                 1 -> Center
                 2 -> Lower
