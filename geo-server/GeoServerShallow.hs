module GeoServerShallow where

import Control.Applicative ( Applicative(liftA2) )

type Point = (Double, Double)

-- Shallow embedding:
-- - more efficient
-- - not as flexible in terms of interpretation
-- - easier to add new kinds of regions
type Region = Point -> Bool

type Radius = Double

type Side = Double

type Direction = (Double, Double)

inRegion :: Point -> Region -> Bool
p `inRegion` r = r p

circle :: Radius -> Region
circle r (x,y) = x**2 + y**2 <= r**2

square :: Side -> Region
square s (x,y) = abs x <= s / 2 && abs y <= s / 2

outside :: Region -> Region
outside r = not . r

(/\) :: Region -> Region -> Region
(/\) = liftA2 (&&)

annulus :: Radius -> Radius -> Region
annulus r1 r2 = outside (circle r1) /\ circle r2

translate :: Direction -> Region -> Region
translate (dx,dy) r (x,y) = r (x-dx, y-dy)

-- toString :: Region -> String
-- toString can't be implemented with this representation of a
-- region since the interpretation is fixed to testing whether
-- or not a point lies in a region. An approximation could be
-- made by sampling the region, but this is will never lead
-- to an accurate description since the "outside" of a region
-- is potentially unbounded.
