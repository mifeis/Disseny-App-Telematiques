import Drawing 

lightBulb c y = colored c (translated 0 (y) (solidCircle 1))
background = colored gray (solidRectangle 2.5 7)
frame = colored black (rectangle 2.5 7)
trafficLight = lightBulb green (-2.3) <> lightBulb yellow 0 <> lightBulb red 2.3 <> frame <> background  

light :: Int -> Int -> Drawing 
light r c = translated (3 * fromIntegral c - 6) (8 * fromIntegral r - 16) trafficLight 

lightRow :: Int -> Drawing
lightRow r = repeatDraw (light r) 3

repeatDraw :: (Int -> Drawing) -> Int -> Drawing
repeatDraw thing 0 = blank
repeatDraw thing n =  (thing n) <> repeatDraw thing (n-1)

myDrawing :: Drawing 
myDrawing = repeatDraw lightRow 3

main :: IO()
main = svgOf myDrawing