{-# LANGUAGE BangPatterns #-}

-- Sutherland-Hodgman polygon clipping

module PolyClip (clipTo, polyLines, (.|)) where
 
import Data.List

import Pt
import Tupelo

type Ln = AB Pt

polyLines :: ConvPoly -> [Ln]
polyLines = linesFrom . polyFrom

-- Return a polygon from a list of points.
polyFrom :: ConvPoly -> ConvPoly
polyFrom ps = last ps : ps
 
-- Return a list of lines from a list of points.
linesFrom :: ConvPoly -> [Ln]
linesFrom poly = zipWith AB poly (tail poly)
 
-- Return true if the point is on or to the left of the oriented line.
(.|) :: Pt -> Ln -> Bool
(.|) !(XY x y) !(AB (XY px py) (XY qx qy)) = (qx-px)*(y-py) >= (qy-py)*(x-px)
{-# INLINE (.|) #-}

-- Return the intersection of two lines.
(><) :: Ln -> Ln -> Pt
(><) !(AB (XY x1 y1) (XY x2 y2)) !(AB (XY x3 y3) (XY x4 y4)) =
    let (r,s) = (x1*y2-y1*x2, x3*y4-y3*x4)
        (t,u,v,w) = (x1-x2, y3-y4, y1-y2, x3-x4)
        d = t*u-v*w 
    in XY ((r*w-t*s)/d) ((r*u-v*s)/d)
{-# INLINE (><) #-}
 
-- Intersect the line segment (p0,p1) with the clipping line's left halfspace,
-- returning the point closest to p1.  In the special case where p0 lies outside
-- the halfspace and p1 lies inside we return both the intersection point and
-- p1.  This ensures we will have the necessary segment along the clipping line.
(-|) :: Ln -> Ln -> ConvPoly
ln@(AB p0 p1) -| clipLn = if in0
    then if in1 then [p1] else [isect]
    else if in1 then [isect, p1] else []
  where
    isect = ln >< clipLn
    in0 = p0 .| clipLn
    in1 = p1 .| clipLn

-- Intersect the polygon with the clipping line's left halfspace.
(<|) :: ConvPoly -> Ln -> ConvPoly
poly <| clipLn = polyFrom $ concatMap (-| clipLn) (linesFrom poly)
 
-- Intersect a target polygon with a clipping polygon.  The latter is assumed to
-- be convex.
clipTo :: ConvPoly -> ConvPoly -> ConvPoly
targPts `clipTo` clipPts = 
    let targPoly = polyFrom targPts
        clipLines = polyLines clipPts
    in tail $ foldl' (<|) targPoly clipLines
