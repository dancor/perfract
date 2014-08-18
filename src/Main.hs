{-# LANGUAGE BangPatterns #-}

-- Computable graphics including some kind of fractals
-- with arbitrary zoom and perfect interpolation.

-- Currenly not supporting overlapping primitives.

import Control.Arrow
import Control.Monad
import Criterion.Main
import Data.Bits
import Data.Time
import Data.Tree
import Data.Word
import qualified Data.Vector.Storable as SVec
import Data.Vector.Storable.ByteString
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import System.Exit

import Affine2D
import PolyClip
import Pt
import RatRot
import Tupelo

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

--  . . . .
-- . o   o  .
-- .o/\  /\o.
--   \/__\/
--    |  |
--    |__|
sqrHair :: RecFig
sqrHair = RecFig
    [ XY 0 0
    , XY (-300) 200
    , XY (-300) 500
    , XY 500 500
    , XY 500 0
    ]
    [XY 0 0, XY 0 200, XY 200 200, XY 200 0]
    [ Prz (XY 0 200)   (ratRot $ -0.11) (0.55)
    , Prz (XY 200 200) (ratRot $ 0.11)  (0.55)
    ]

winW :: Int
winW = 768

winH :: Int
winH = 768

polyA :: AugM -> ConvPoly -> ConvPoly
polyA a = map (applyA a)

data Model = Model
    { mTime :: Float
    , mNum :: Int
    } deriving Show

myPic :: Model -> IO Picture
myPic (Model t n) = do
    return $ bitmapOfByteString winW winH (vectorToByteString myVec') True

updateModel :: ViewPort -> Float -> Model -> IO Model
updateModel _ dT (Model t n) = return $ Model (t + dT) (n + 1)

myVec' :: SVec.Vector Word32
myVec' = myVec winW winH

myVec :: Int -> Int -> SVec.Vector Word32
myVec !w !h = SVec.generate (h * w) (coordVal . offsetToCoord)
  where
    offsetToCoord i = i `quotRem` w
    coordVal (y, x) = ((r `shift` 8 .|. g) `shift` 8 .|. b) `shift` 8 .|. a
      where
        r = 0
        g = -- if x < 100 && y < 100 then
            fromIntegral $ polyPixel 1 (fromIntegral x) (fromIntegral y)
            [XY 0 0, XY 50 0, XY 1 50]
            -- else fromIntegral x `rem` 256
        b = 0
        a = 255

main :: IO ()
main = do
    {-
    defaultMain [
      bgroup "sqr" [bench "30" $ nf (myVec 30) 30]
      ]
      -}
    t1 <- getCurrentTime
    t2 <- myVec' `seq` getCurrentTime
    print (realToFrac $ diffUTCTime t2 t1)
    simulateIO (InWindow "Lol" (winW, winH) (0, 0)) (greyN 0.5)
        1 (Model 0 0) myPic updateModel

doPrz :: PosRotZoom -> AugM -> AugM
doPrz (Prz p r z) = translateA p . rotateA r . scaleA z

figStep :: RecFig -> AugM -> (ConvPoly, [AugM])
figStep !fig !a = (polyA a (rPoly fig), map (flip doPrz a) $ rPrzs fig)

{-
-- Breadth-first necessary for arbitrary-depth.
figSteps :: RecFig -> AugM -> ([ConvPoly], [AugM])
figSteps _ 0 !a = ([], [a])
figSteps !fig 1 !a = first (:[]) (figStep fig a)
figSteps !fig !n !a =
    first (map (applyA a) (rPoly fig) :) .
    first concat . second concat . unzip $ map (figSteps fig (n - 1)) augMs
  where
    (poly, augMs) = figStep fig a
-}

-- Depth-first should be fastest when doing a batch to a fixed depth.
figStepN :: RecFig -> Int -> AugM -> ([ConvPoly], [AugM])
figStepN _ 0 !a = ([], [a])
figStepN !fig 1 !a = first (:[]) (figStep fig a)
figStepN !fig !n !a =
    first (map (applyA a) (rPoly fig) :) .
    first concat . second concat . unzip $ map (figStepN fig (n - 1)) augMs
  where
    (poly, augMs) = figStep fig a

polyArea :: ConvPoly -> Rational
polyArea poly = sum (map ptDet $ polyLines poly) / 2
  where
    ptDet (AB (XY x1 y1) (XY x2 y2)) = x1 * y2 - x2 * y1

-- Return true if the point is in (or on the border of) the polygon.
isIn :: Pt -> ConvPoly -> Bool
isIn pt = all (pt .|) . polyLines

polyPixel :: Rational -> Rational -> Rational -> ConvPoly -> Int
polyPixel w x y poly = compute
    {-
    if pixTopLIn
      then if pixTopRIn && pixBotLIn && pixBotRIn then 255 else compute
      else if not (pixTopRIn || pixBotLIn || pixBotRIn) then 0 else compute
      -}
  where
    pixTopL = XY x y
    pixTopR = XY xw y
    pixBotL = XY x yw
    pixBotR = XY xw yw
    pixTopLIn = isIn pixTopL poly
    pixTopRIn = isIn pixTopR poly
    pixBotLIn = isIn pixBotL poly
    pixBotRIn = isIn pixBotR poly
    xw = x + w
    yw = y + w
    isectPoly = poly `clipTo` [XY x y, XY xw y, XY xw yw, XY x yw]
    compute = case isectPoly of
      Nothing -> 0
      Just poly -> round $ 255 * polyArea poly / w / w

{-
-- Is the point left (LT) of the line thru p then q.
-- So EQ means in the closed halfplane of the line and LT means strictly in.
(.|) :: Pt -> Line -> Ordering
(x, y) .| ((px, py), (qx, qy)) = compare
    ((x - px) * (qy - py))
    ((y - py) * (qx - px))

-- At least 1 element must fail the predicate.
findBookendedSegmentCanWrap :: (a -> Bool) -> [a] -> (a, [a], a)
findBookendedSegmentCanWrap p xs = (bookend1, yeses, bookend2)
    (nos, yesesRest) = break p . dropWhile p $ xs ++ xs
    bookend1:_ = reverse nos
    (yeses, rest) = span p yesesRest
    bookend2 = head rest

-- Intersect a convex polygon by a halfplane.
(|>) :: ConvPoly -> Line -> Maybe ConvPoly
[] |> _ = error "Degenerate polygon."
[_] |> _ = error "Degenerate polygon."
pts |> l =
    if all (/= LT) ptsIn then Nothing else
    if all (/= GT) ptsIn then Just pts else
  where
    ptsIn = map (.| l) pts
    (out1, ins, out2) =
        findBookendedSegmentCanWrap ((/= GT) . snd) $ zip pts ptsIn
    new1 = 
    findInSegment :: [(Pt, Ordering)]
        -> ((Pt, Ordering), [(Pt, Ordering)], (Pt, Ordering)
    findInSegment :: [(Pt, Ordering)]
-}

