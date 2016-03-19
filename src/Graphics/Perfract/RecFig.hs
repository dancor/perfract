module Graphics.Perfract.RecFig where

import qualified Data.Vector as Vec

import Graphics.Perfract.PolyClip
import Graphics.Perfract.RatRot
import Graphics.Perfract.Tupelo

data RecFig = RecFig
    -- { rBound :: !(ConvPoly Rational)
    { rPoly :: !(ConvPoly Rational)
    , rPrzs :: !(Vec.Vector PosRotZoom)
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
