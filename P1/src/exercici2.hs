import Drawing

lightBulb c y = colored c (translated 0 (y) (solidCircle 1))
frame = colored gray (solidRectangle 2.5 8.5)
trafficLight = lightBulb green (-3) <> lightBulb yellow 0 <> lightBulb red 3 <> frame  

myDrawing :: Drawing
myDrawing = trafficLight

main:: IO()
main = svgOf myDrawing