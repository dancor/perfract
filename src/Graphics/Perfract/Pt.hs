module Graphics.Perfract.Pt where

import Graphics.Perfract.Tupelo

type Pt = XY Rational

type Ln = AB Pt

-- | Note: Do not repeat the initial vertex at the end.
-- | Points in counterclockwise order.
-- Should this be strict?
type ConvPoly = [Pt]
