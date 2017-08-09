module GUIBoard where

import Graphics.UI.WX
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.Draw

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  selected <- varCreate Nothing
  
  let img = image "images/bishopb.gif"
  let bmap = bitmap "images/bishopb.gif"
  let xsize = 96
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
