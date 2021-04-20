
{-# LANGUAGE OverloadedStrings #-}

import Drawing

main :: IO ()
main = activityOf 60 30 initial handle draw

initial = 0

handle (KeyDown " ") angle = angle + pi / 4
handle _             angle = angle

draw angle =
    rotated angle $ translated 2 0 (colored red $ solidCircle 0.5) <> polyline [(0, 0), (2, 0)]

