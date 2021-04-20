import Drawing

line = polyline [(0,0),(0,1)]

branch :: Int -> Drawing
branch 0 = flower
branch n = rotated (pi/10) (line <> translated 0 1 (branch (n-1))) <> rotated (-pi/10) (line <> translated 0 1 (branch (n-1)))

flower = colored yellow (solidCircle 0.2)

main :: IO()
main = svgOf (line <> translated 0 1 (branch 7))


