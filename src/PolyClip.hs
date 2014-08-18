{-# LANGUAGE BangPatterns #-}

-- Sutherland-Hodgman polygon clipping

module PolyClip (clipTo, polyLines, (.|)) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import Pt
import Tupelo

type Ln = AB Pt

polyLines :: ConvPoly -> [Ln]
polyLines x = linesFrom . selfComplete $!! x

-- Return a self-completed polygon from a list of points.
selfComplete :: [Pt] -> [Pt]
selfComplete ps = ps `deepseq` (last ps : ps)
 
-- Return all polygon lines from the self-complete point list.
linesFrom :: [Pt] -> [Ln]
linesFrom ps = ps `deepseq` (zipWith AB ps (tail ps))
 
-- Return true if the point is on or to the left of the oriented line.
(.|) :: Pt -> Ln -> Bool
(.|) !(XY x y) !(AB (XY px py) (XY qx qy)) = (qx-px)*(y-py) >= (qy-py)*(x-px)
{-# INLINE (.|) #-}

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
(-|) :: Ln -> Ln -> [Pt]
(-|) !ln@(AB p0 p1) !clipLn = if in0
    then if in1 then [p1] else [isect]
    else if in1 then [isect, p1] else []
  where
    isect = ln >< clipLn
    in0 = p0 .| clipLn
    in1 = p1 .| clipLn

-- Intersect the polygon with the clipping line's left halfspace.
(<|) :: [Pt] -> Ln -> Maybe [Pt]
(<|) poly !clipLn =
  let res = deepseq poly $ concatMap (-| clipLn) (linesFrom poly)
  in case res of
    [] -> Nothing
    _ -> Just $ selfComplete res
 
-- Intersect a polygon with a clipping polygon.
clipTo :: ConvPoly -> ConvPoly -> Maybe ConvPoly
clipTo poly clip =
    deepseq poly $ deepseq clip $
    tail <$> (foldM (<|) (selfComplete poly) (polyLines clip))
