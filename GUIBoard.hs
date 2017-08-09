module GUIBoard where

import Graphics.UI.WX
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.Draw
import System.FilePath (FilePath, (</>))
import Pieces

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  selected <- varCreate Nothing

  let Single bmap = getBitmap $ Piece Black (Prime G)
  let onPaint1 dc viewArea = do
        drawRect dc (Rect (xsize-1) (xsize-1) (8*xsize+2) (8*xsize+2)) [ ]
        let drawField col row = do
              let colour = if (col + row) `mod` 2 == 0 then grey else white
              drawRect dc (Rect (xsize*row) (xsize*col) xsize xsize) [ brush := brushSolid colour ]
        sequence_ [drawField col row | col <- [1..8], row <- [1..8]]

        sequence_ [drawText dc (show (9-col))
                   (Point (xsize `div` 2) (xsize * col + xsize `div` 2)) [] | col <- [1..8]]
        
        sequence_ [drawText dc [" abcdefgh " !! row]
                   (Point (xsize * row + xsize `div` 2) (xsize * 9 + xsize `div` 2)) [] | row <- [1..8]]

  let onPaint dc viewArea = do
        onPaint1 dc viewArea
        sz <- get bmap size
        drawBitmap dc bmap (centerField (read "d8") sz) True []
        drawBitmap dc bmap (upperField (read "c7") sz) True []
        drawBitmap dc bmap (lowerField (read "c6") sz) True []
        drawBitmap dc bmap (upperField (read "c6") sz) True []
        p <- varGet selected
        case p of
          Just pos -> do
            drawBitmap dc bmap pos True [] 
            circle dc pos 20 [brush := brushSolid red]
          Nothing -> return ()

  let onClick point = varSet selected $ Just point
  let onUnclick point = varSet selected Nothing
  let onDrag point = do
        putStrLn $ "on mouse: " ++ show point
        p <- varGet selected
        case p of
          Nothing -> return ()
          Just _ -> varSet selected (Just point)
  
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

centerField (Field (f, r)) (Size w h) =
  Point x y where
    x = f*xsize + (xsize - w) `div` 2
    y = (9-r)*xsize + (xsize - h) `div` 2

upperField (Field (f, r)) (Size w h) =
  Point x y where
    x = f*xsize + (xsize - w) `div` 2
    y = (9-r)*xsize + (xsize*2 - h*3) `div` 6

lowerField (Field (f, r)) (Size w h) =
  Point x y where
    x = f*xsize + (xsize - w) `div` 2
    y = (9-r)*xsize + (xsize*4 - h*3) `div` 6
