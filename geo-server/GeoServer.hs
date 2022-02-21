module GeoServer where

data Point = Point Float Float

type Region = Point -> Bool

type Radius = Float

inRegion :: Point -> Region -> Bool
p `inRegion` r = r p

circle :: Radius -> Region
circle r (Point x y) = x**2 + y**2 < r**2

outside :: Region -> Region
outside = (not .)

(/\) :: Region -> Region -> Region
r1 /\ r2 = (&&) <$> r1 <*> r2

annulus :: Radius -> Radius -> Region
annulus r1 r2 = outside (circle r1) /\ circle r2
