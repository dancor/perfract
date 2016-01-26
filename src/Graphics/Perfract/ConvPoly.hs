module Graphics.Perfract.ConvPoly where

import Graphics.Perfract.Pt
import qualified Data.Vector as Vec

-- | Note: Do not repeat the initial vertex at the end.
-- | Points in counterclockwise order.
-- Should this be strict?
type ConvPoly = Vec.Vector Pt

{-
polyArea :: ConvPoly -> Rational
polyArea poly = Vec.sum (Vec.map ptDet $ polyLines poly) / 2
  where
    ptDet (AB (XY x1 y1) (XY x2 y2)) = x1 * y2 - x2 * y1
-}
