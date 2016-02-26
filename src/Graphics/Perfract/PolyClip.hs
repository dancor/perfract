{-# LANGUAGE BangPatterns #-}

-- Sutherland-Hodgman polygon clipping

module Graphics.Perfract.PolyClip
  ( clipTo
  , ConvPoly
  , polyArea
  , polyLines
  , (.|)
  , (.@)
  ) where

import Control.Applicative
import qualified Data.Vector as Vec

import Graphics.Perfract.Pt
import Graphics.Perfract.Tupelo

-- | Note: Do not repeat the initial vertex at the end.
-- | Points in counterclockwise order.
-- Should this be strict?
type ConvPoly = Vec.Vector Pt

polyArea :: ConvPoly -> Rational
polyArea poly = Vec.sum (Vec.map ptDet $ polyLines poly) / 2
  where
    ptDet (AB (XY x1 y1) (XY x2 y2)) = x1 * y2 - x2 * y1
type PtV = Vec.Vector Pt

type LnV = Vec.Vector Ln

polyLines :: ConvPoly -> LnV
polyLines !x = linesFrom $ selfComplete x

-- Return a self-completed polygon from a list of points.
selfComplete :: ConvPoly -> PtV
-- selfComplete ps = ps `deepseq` (last ps : ps)
selfComplete ps =
    if pLast == Vec.head ps then ps else Vec.cons pLast ps
  where
    pLast = Vec.last ps

-- Return all polygon lines from the self-complete point list.
linesFrom :: PtV -> LnV
linesFrom ps = Vec.zipWith AB ps (Vec.tail ps)

-- Return true if the point is on or to the left of the oriented line.
(.|) :: Pt -> Ln -> Bool
(.|) !(XY x y) !(AB (XY px py) (XY qx qy)) = (qx-px)*(y-py) >= (qy-py)*(x-px)
{-# INLINE (.|) #-}

-- Return true if the point is in (or on the border of) the polygon.
(.@) :: Pt -> ConvPoly -> Bool
(.@) pt = Vec.all (pt .|) . polyLines

-- Return the intersection of two lines.
-- Parallel lines will give a fatal exception.
-- (We always know that lines aren't parallel the one time we call this and
-- don't want to waste time checking anyway.)
(><) :: Ln -> Ln -> Pt
(><) !(AB (XY x1 y1) (XY x2 y2)) !(AB (XY x3 y3) (XY x4 y4)) =
    let x12 = x1 - x2
        y34 = y3 - y4
        y12 = y1 - y2
        x34 = x3 - x4
        d12 = x1 * y2 - y1 * x2
        d34 = x3 * y4 - y3 * x4
        d = x12 * y34 - y12 * x34
    in XY ((d12 * x34 - d34 * x12) / d) ((d12 * y34 - d34 * y12) / d)
{-# INLINE (><) #-}

-- Intersect the line segment (p0,p1) with the clipping line's left halfspace,
-- returning the point closest to p1.  In the special case where p0 lies outside
-- the halfspace and p1 lies inside we return both the intersection point and
-- p1.  This ensures we will have the necessary segment along the clipping line.
(-|) :: Ln -> Ln -> PtV
(-|) !ln@(AB p0 p1) !clipLn = if in0
    then Vec.singleton $ if in1 then p1 else isect
    else if in1
      then if isect == p1 then Vec.singleton p1 else Vec.fromList [isect, p1]
      else Vec.empty
  where
    isect = ln >< clipLn
    in0 = p0 .| clipLn
    in1 = p1 .| clipLn

-- Intersect the polygon with the clipping line's left halfspace.
(<|) :: PtV -> Ln -> Maybe PtV
(<|) !poly !clipLn =
  let res = Vec.concatMap (-| clipLn) (linesFrom poly)
  in if Vec.null res then Nothing else Just $ selfComplete res

-- Intersect a polygon with a clipping polygon.
clipTo :: ConvPoly -> ConvPoly -> Maybe ConvPoly
clipTo !poly !clip =
    Vec.tail <$> (Vec.foldM (<|) (selfComplete poly) (polyLines clip))
