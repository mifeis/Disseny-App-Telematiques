import Drawing

lightBulb c y = colored c (translated 0 (y) (solidCircle 1))
frame = colored gray (solidRectangle 2.5 8.5)
trafficLight = lightBulb green (-3) <> lightBulb yellow 0 <> lightBulb red 3 <> frame

lights :: Int -> Drawing
lights 0 = blank
lights n = trafficLight <> translated 3 0 (lights (n-1))

myDrawing :: Drawing
myDrawing = lights 3

main:: IO()
main = svgOf myDrawing
