import Drawing 

lightBulb c y = colored c (translated 0 (y) (solidCircle 1))
background = colored gray (solidRectangle 2.5 7)
frame = colored black (rectangle 2.5 7)
trafficLight = lightBulb green (-2.3) <> lightBulb yellow 0 <> lightBulb red 2.3 <> frame <> background


trafficLigths :: [(Double, Double)] -> Drawing
trafficLigths array = foldMap translate array

translate :: (Double, Double) -> Drawing
translate (x,y) = translated x y (trafficLight) 

array :: [(Double, Double)]
array = [(-3.0, 7.5), (-3.0,0.0), (-3.0,-7.5), (0.0, 7.5), (0.0, 0.0), (0.0,-7.5), (3.0, 7.5), (3.0, 0.0), (3.0, -7.5)]

myDrawing :: Drawing
myDrawing = trafficLigths array

main :: IO()
main = svgOf myDrawing 
