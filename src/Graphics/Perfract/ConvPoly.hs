module Graphics.Perfract.ConvPoly where

import Graphics.Perfract.Pt
import qualified Data.Vector as Vec

-- | Note: Do not repeat the initial vertex at the end.
-- | Points in counterclockwise order.
-- Should this be strict?
type ConvPoly = Vec.Vector Pt
