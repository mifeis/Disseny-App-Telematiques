
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }

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
    }

-----------------------------------------------------
-- Event processing

handleEvent :: Event -> Game -> Game

handleEvent (KeyDown "N") game =                -- Next generation
    setGmBoard (nextGeneration (gmBoard game)) game

handleEvent (MouseDown (x, y)) game =           -- Set live/dead cells
    let pos = (round x, round y)
        brd = gmBoard game
    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game

handleEvent (KeyDown "G") game = 		-- Set grid type
    setGmGridMode (setGrid (gmGridMode game)) game

handleEvent _ game =                            -- Ignore other events
    game

-------------------------------------------------------
--Set Grid

setGrid :: GridMode -> GridMode
setGrid NoGrid = LivesGrid
setGrid LivesGrid = ViewGrid
setGrid ViewGrid = NoGrid
 

-----------------------------------------------------
-- Drawing

draw game =
    drawBoard (gmBoard game) <>
    case (gmGridMode game) of
	NoGrid -> blank
	LivesGrid -> drawGrid (minLiveCell (gmBoard game)) (maxLiveCell (gmBoard game))
	ViewGrid -> drawGrid (round(-viewWidth), round(-viewHeight)) (round(viewWidth), round(viewHeight))


