import Drawing

lightBulb c y = colored c (translated 0 (y) (solidCircle 1))
solid = colored gray (solidRectangle 2.5 7)
frame = colored black (rectangle 2.5 7)
trafficLight = lightBulb green (-2.3) <> lightBulb yellow 0 <> lightBulb red 2.3 <> frame <> solid

lights :: Int -> Drawing
lights 0 = blank
lights n = trafficLight <> translated 3 0 (lights (n-1))

columns :: Double -> Drawing
columns 0 = blank
columns m = translated (-3) 0 (lights 3) <> translated 0 8 (columns (m-1))

myDrawing :: Drawing
myDrawing = translated 0 (-9) (columns 3)

main :: IO()
main = svgOf myDrawing