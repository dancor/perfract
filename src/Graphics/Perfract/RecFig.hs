module Graphics.Perfract.RecFig where

import Graphics.Perfract.Pt
import Graphics.Perfract.RatRot
import Graphics.Perfract.Tupelo

data RecFig = RecFig
    { rBound :: !ConvPoly
    , rPoly :: !ConvPoly
    , rPrzs :: ![PosRotZoom]
    }

data PosRotZoom = Prz
    -- pPos is the offset point before applying rZoom.
    { pPos :: !(XY Rational)
    , pRot :: !RatRot
    -- pZoom should be > 0 and < 1.
    , pZoom :: !Rational
    }

--rfPopN :: RecFig -> RecFig
--rfPopN (RecFig 
