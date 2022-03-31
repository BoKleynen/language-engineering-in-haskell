{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import GeoServerDeep


toShallow :: Region -> TExpQ (Point -> Bool)
toShallow region = [|| \(x,y) -> $$(go region [|| x ||] [|| y ||]) ||]
  where
    go :: Region -> TExpQ Double -> TExpQ Double -> TExpQ Bool
    go (Circle r) x y = [|| $$x**2 + $$y**2 <= $$(liftTyped (r**2)) ||]
    go (Square s) x y = [|| abs $$x <= $$(liftTyped (s / 2)) && abs $$y <= $$(liftTyped (s / 2)) ||]
    go (Translate (dx,dy) a) x y = [||
        let x' = $$x - dx
            y' = $$y - dy
        in $$(go a  [|| x' ||] [|| y' ||])
      ||]
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
        intersection a1'          a2'        = [|| $$(go a1'  x y) && $$(go a2'  x y) ||]
