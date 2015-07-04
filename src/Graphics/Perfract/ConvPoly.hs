module Graphics.Perfract.ConvPoly where

import Graphics.Perfract.Pt

-- | Note: Do not repeat the initial vertex at the end.
-- | Points in counterclockwise order.
-- Should this be strict?
type ConvPoly = [Pt]
