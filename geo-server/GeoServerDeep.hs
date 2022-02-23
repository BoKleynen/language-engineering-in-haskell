module GeoServerDeep where

type Radius = Float

type Point = (Float, Float)

-- Deep embedding
-- - less efficient
-- - easy to add new interpretations (e.g.: SVG representation)
-- - more work to add new kinds of regions => would have to update all pattern matches
data Region
    = Circle Radius
    | Intersection Region Region
    | Outside Region

inRegion :: Point -> Region -> Bool
inRegion (x,y) (Circle r) = x**2 + y**2 <= r**2
inRegion p (Intersection r1 r2) = inRegion p r1 && inRegion p r2
inRegion p (Outside r) = not (inRegion p r)

annulus :: Radius -> Radius -> Region
annulus r1 r2 = Outside (Intersection (Circle r1) (Circle r2))
