import Drawing

botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0 1.5 (solidCircle 1))
frame = rectangle 2.5 5.5
trafficLight = botCircle green <> topCircle red <> frame

myDrawing :: Drawing
myDrawing = trafficLight

main:: IO()
main = svgOf myDrawing