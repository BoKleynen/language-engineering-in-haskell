module GeoServerDeep where

type Point = (Double, Double)

type Direction = (Double, Double)

type Radius = Double

type Side = Double

-- Deep embedding
-- - less efficient
-- - easy to add new interpretations (e.g.: SVG representation)
-- - more work to add new kinds of regions => would have to update all pattern matches
data Region
  = Circle Radius
  | Intersection Region Region
  | Outside Region
  | Square Side
  | Translate Direction Region
  deriving (Show, Eq)

inRegion :: Point -> Region -> Bool
inRegion (x,y) (Circle r) = x**2 + y**2 <= r**2
inRegion p (Intersection r1 r2) = inRegion p r1 && inRegion p r2
inRegion p (Outside r) = not (inRegion p r)
inRegion (x,y) (Square s) = abs x <= s / 2 && abs y <= s / 2
inRegion (x,y) (Translate (dx,dy) r) = inRegion (x-dx,y-dy) r

circle :: Radius -> Region
circle = Circle

square :: Side -> Region
square = Square

outside :: Region -> Region
outside = Outside

(/\) :: Region -> Region -> Region
(/\) = Intersection

annulus :: Radius -> Radius -> Region
annulus r1 r2 = outside (circle r1) /\ circle r2

translate :: Direction -> Region -> Region
translate = Translate

toString :: Region -> String
toString = show
