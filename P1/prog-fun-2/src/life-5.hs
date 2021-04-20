
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing
import Drawing.Vector

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        , gmZoom :: Double, gmShift :: Point
        , gmPaused :: Bool
        , gmInterval :: Time    -- generation interval when not paused
        , gmElapsedTime :: Time -- elapsed time from last generation
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }
setGmZoom x g        = g{ gmZoom = x }
setGmShift x g       = g{ gmShift = x }
setGmPaused x g      = g{ gmPaused = x }
setGmInterval x g    = g{ gmInterval = x }
setGmElapsedTime x g = g{ gmElapsedTime = x }

data GridMode = NoGrid | LivesGrid | ViewGrid
    deriving (Show, Read)

-----------------------------------------------------
-- Initialization

viewWidth, viewHeight :: Double
viewWidth = 60.0
viewHeight = 30.0

main :: IO ()
main =
    activityOf viewWidth viewHeight initial handleEvent draw

board0Cells =
    [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial = Game
    { gmBoard = foldr (setCell True) initBoard board0Cells
    , gmGridMode = NoGrid
    , gmZoom = 1.0, gmShift = (0.0, 0.0)
    , gmPaused = True
    , gmInterval = 1.0 -- in seconds
    , gmElapsedTime = 0.0
    }

-----------------------------------------------------
-- Event processing 

handleEvent :: Event -> Game -> Game

handleEvent (KeyDown "N") game = 		-- Next generation
    setGmBoard (nextGeneration (gmBoard game)) game

handleEvent (MouseDown (x, y)) game = 		-- Set live/dead cells
    let pos = pointToPos (x, y) game
        brd = gmBoard game
    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game

handleEvent (KeyDown "G") game =		-- Set gride type  
    setGmGridMode (setGrid (gmGridMode game)) game 
 
handleEvent (KeyDown "I") game = 		-- Zoom In 
    if gmZoom game < 2.0 then setGmZoom (gmZoom game * 2.0) game
    else game 

handleEvent (KeyDown "O") game = 		-- Zoom Out 
    if gmZoom game > 0.1 then setGmZoom (gmZoom game / 2.0) game 
    else game

handleEvent (KeyDown "ARROWUP") game = 		-- Down Shift 
    setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (0,5)) game 

handleEvent (KeyDown "ARROWDOWN") game = 	-- Up Shift 
    setGmShift (gmShift game ^+^ (1.0 / gmZoom game) *^ (0,5)) game

handleEvent (KeyDown "ARROWLEFT") game = 	-- Right Shift 
    setGmShift (gmShift game ^+^ (1.0 / gmZoom game) *^ (5,0)) game

handleEvent (KeyDown "ARROWRIGHT") game = 	-- Left Shift
    setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (5,0)) game

handleEvent (KeyDown " ") game = 		-- Pause game
    setGmPaused (setPause (gmPaused game)) game

handleEvent (KeyDown "+") game = 		-- Automatic generation goes faster
    if gmInterval game > 0.125 then setGmInterval (gmInterval game / 2.0) game
    else game

handleEvent (KeyDown "-") game = 		-- Automatic generation goes slower
    if gmInterval game < 1.0 then setGmInterval (gmInterval game * 2.0) game
    else game

handleEvent (TimePassing dt) game = 		-- the pass of time
    if (gmElapsedTime game + dt) >= (gmInterval game) && (gmPaused game == False) then setGmElapsedTime 0 (setGmBoard (nextGeneration (gmBoard game)) (game))
    else setGmElapsedTime (gmElapsedTime game + dt) game

handleEvent _ game = 				-- Ignore other events
    game 

--------------------------------------------------------
--Set Grid 

setGrid :: GridMode -> GridMode
setGrid NoGrid = LivesGrid
setGrid LivesGrid = ViewGrid
setGrid ViewGrid = NoGrid

---------------------------------------------------------
--Set Pause

setPause :: Bool -> Bool
setPause True = False
setPause False = True

--------------------------------------------------------
--Point to position 

pointToPos :: Point -> Game -> Pos
pointToPos p game = 
    let (gx, gy) = (1.0 / gmZoom game) *^ p ^-^ gmShift game
    in (round gx, round gy)

-------------------------------------------------------
--Menu drawing 

column1 = ["N", "G", "O", "I", "ARROWUP", "ARROWDOWN", "ARROWRIGHT", "ARROWLEFT", "SPACE", "+", "-", "Use the mouse to set live/dead cells"]
column2 = ["Next step", "Change grid mode", "Zoom out", "Zoom in", "Shift up", "Shift down", "Shift left", "Shift right", "Pause/run toggle", "Increase run velocity", "Decrease run velocity"]
column3 :: Game -> [String]
column3 game = [show (1.0/gmInterval game) ++ " steps per second" ++ (if (gmPaused game) then " (paused)" else ""), "Zoom = " ++ show (gmZoom game), "Shift = (" ++ show (fst (gmShift game)) ++ "," ++ show (snd (gmShift game)) ++ ")"]

drawLine :: [String] -> Color -> String -> Drawing
drawLine [] c s = blank
drawLine (l : ls) c s = colored c (translated 0 (-1) ((atext s l) <> drawLine ls c s))

--------------------------------------------------------
-- Drawing 

draw game = 
    (if gmPaused game then
    translated (-viewWidth/2.2) (viewHeight/2.2) (drawLine column1 blue startAnchor) <>
    translated (-viewWidth/2.2 + 5) (viewHeight/2.2) (drawLine column2 blue startAnchor)
    else blank) <>
    translated (viewWidth/2.2) (viewHeight/2.2)  (drawLine (column3 game) red endAnchor) <>
    scaled (gmZoom game) (gmZoom game)
    (translated (fst(gmShift game)) (snd(gmShift game))
    (drawBoard (gmBoard game) <> 
    case (gmGridMode game) of 
        NoGrid -> blank
        LivesGrid -> drawGrid (minLiveCell (gmBoard game)) (maxLiveCell(gmBoard game))
        ViewGrid -> drawGrid (pointToPos (-viewWidth,-viewHeight) game) (pointToPos (viewWidth,viewHeight) game))) 






