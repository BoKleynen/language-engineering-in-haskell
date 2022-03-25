{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import GeoServerDeep


toShallow :: Region -> TExpQ (Point -> Bool)
toShallow (Circle r)            = [|| \(x,y) -> x**2 + y**2 <= $$(liftTyped (r**2)) ||]
toShallow (Square s)            = [|| \(x,y) -> abs x <= $$(liftTyped (s / 2)) && abs y <= $$(liftTyped (s / 2)) ||]
toShallow (Translate (dx,dy) a) = [|| \(x,y) -> $$(toShallow a) (x-dx, y-dy) ||]
toShallow (Outside a)           = case a of
                                    (Outside a') -> toShallow a'
                                    _            -> [|| not . $$(toShallow a) ||]
toShallow (Intersection a1 a2)  = go a1 a2
  where
    go (Circle r1) (Circle r2) = [|| $$(toShallow (Circle (min r1 r2))) ||]
    go (Square s1) (Square s2) = [|| $$(toShallow (Square (min s1 s2))) ||]
    go (Square r)  (Circle s) | r <= s / 2      = [|| $$(toShallow (Circle r)) ||]
                              | s <= r * sqrt 2 = [|| $$(toShallow (Square s)) ||]
    go (Circle s)  (Square r) | r <= s / 2      = [|| $$(toShallow (Circle r)) ||]
                              | s <= r * sqrt 2 = [|| $$(toShallow (Square s)) ||]
    go _           _           = [|| liftA2 (&&) $$(toShallow a1) $$(toShallow a2) ||]


toShallow2 :: Region -> TExpQ (Point -> Bool)
toShallow2 region = [|| \(x,y) -> $$(go region [|| x ||] [|| y ||]) ||]
  where
    go :: Region -> TExpQ Double -> TExpQ Double -> TExpQ Bool
    go (Circle r) x y = [|| $$x**2 + $$y**2 <= $$(liftTyped (r**2)) ||]
    go (Square s) x y = [|| abs $$x <= $$(liftTyped (s / 2)) && abs $$y <= $$(liftTyped (s / 2)) ||]
    go (Translate (dx,dy) a) x y = [|| $$(go a [|| $$x - dx ||] [|| $$y - dy ||]) ||]
    go (Outside a) x y = case a of
                            (Outside a') -> go a' x y
                            _            -> [|| not $$(go a x y) ||]
    go (Intersection a1 a2) x y  = intersection a1 a2
      where
        intersection :: Region -> Region -> TExpQ Bool
        intersection (Circle r1) (Circle r2) = [|| $$(go (Circle (min r1 r2)) x y) ||]
        intersection (Square s1) (Square s2) = [|| $$(go (Square (min s1 s2)) x y) ||]
        intersection (Square r)  (Circle s) | r <= s / 2      = [|| $$(go (Circle r) x y) ||]
                                            | s <= r * sqrt 2 = [|| $$(go (Square s) x y) ||]
        intersection (Circle s)  (Square r) | r <= s / 2      = [|| $$(go (Circle r) x y) ||]
                                            | s <= r * sqrt 2 = [|| $$(go (Square s) x y) ||]
        intersection _           _           = [|| $$(go a1  x y) && $$(go a2  x y) ||]
