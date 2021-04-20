
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

--------------------------------------------------------
-- Drawing 

draw game = 
    translated (fst(gmShift game)) (snd(gmShift game))
    (scaled (gmZoom game) (gmZoom game)
    (drawBoard (gmBoard game) <>
    case (gmGridMode game) of 
        NoGrid -> blank
        LivesGrid -> drawGrid (minLiveCell (gmBoard game)) (maxLiveCell(gmBoard game))
        ViewGrid -> drawGrid (round(-viewWidth / (gmZoom game)),round(-viewHeight / (gmZoom game))) (round(viewWidth /(gmZoom game)), round(viewHeight /(gmZoom game))))) 






