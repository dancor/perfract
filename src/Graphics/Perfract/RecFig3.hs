module Graphics.Perfract.RecFig3 where

import qualified Data.Vector as Vec

import Graphics.Perfract.RatRot
import Graphics.Perfract.Shape
import Graphics.Perfract.Tupelo

data RecFig3 = RecFig3
    { rShape :: !Shape
    , rPrz3s :: !(Vec.Vector PosRotZoom3)
    }

data PosRotZoom3 = Prz3
    -- pPos is the offset point before applying rZoom.
    { p3Pos :: !(XYZ Rational)
    -- Only doing x-rotations to start with.
    , p3XRot :: !RatRot
    , p3Zoom :: !Rational
    }
