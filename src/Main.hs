{-# LANGUAGE BangPatterns #-}

-- Computable graphics including some kind of fractals
-- with arbitrary zoom and perfect interpolation.

-- Currenly not supporting overlapping primitives.

import Control.Applicative
import Control.Arrow
import Control.Monad
import Criterion.Main
import Data.Bits
import Data.List
import Data.Time
import Data.Word
import qualified Data.Vector.Storable as SVec
import qualified Data.Vector.Storable.Mutable as SVecM
import Data.Vector.Storable.ByteString
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate

import Affine2D
import PolyClip
import Pt
import RatRot
import Tupelo

type PolyBox a = XY (AB a)

toIntBox :: PolyBox Rational -> PolyBox Int
toIntBox !(XY (AB x1 x2) (AB y1 y2)) =
    XY (AB (floor x1) (ceiling x2)) (AB (floor y1) (ceiling y2))

polyGetBox :: ConvPoly -> PolyBox Rational
polyGetBox (XY x1 y1 : XY x2 y2 : rest) = foldl' trav (XY x12 y12) rest
  where
    x12 = if x1 <= x2 then AB x1 x2 else AB x2 x1
    y12 = if y1 <= y2 then AB y1 y2 else AB y2 y1
    trav !(XY xs ys) !(XY x y) = XY (incl x xs) (incl y ys)
    incl !z !a@(AB zMin zMax) =
      if z <= zMin then AB z zMax else
      if z >= zMax then AB zMin z else a
polyGetBox _ = error "polyGetBox: invalid poly"

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
    , Prz (XY 200 200) (ratRot $ 0.17)  (0.55)
    ]

winW :: Int
winW = 768

winH :: Int
winH = 768

polyA :: AugM -> ConvPoly -> ConvPoly
polyA a = map (applyA a)

myPic :: SVec.Vector Word32 -> a -> IO Picture
myPic v _ = return $ bitmapOfByteString winW winH (vectorToByteString v) True

canv8Blank :: Int -> Int -> SVec.Vector Word8
canv8Blank w h = SVec.replicate (h * w) 0

canv8To32 :: SVec.Vector Word8 -> SVec.Vector Word32
canv8To32 = SVec.map f
  where
    f x = ((r `shift` 8 .|. g) `shift` 8 .|. b) `shift` 8 .|. a
      where
        r = 0
        g = fromIntegral x
        b = 0
        a = 255

canvAdd :: SVecM.IOVector Word8 -> (ConvPoly, PolyBox Int) -> IO ()
canvAdd canv (poly, XY (AB x1 x2) (AB y1 y2)) =
    doLines y1 (y1 * winW)
  where
    doLines y i = if y == y2
      then return ()
      else polyLine poly y x1 x2 canv i >> doLines (y + 1) (i + winW)
    {-
    doLine y x i = if x == x2
      then return ()
      else doPt y x i >> doLine y (x + 1) (i + 1)
    doPt y x i = do
        a <- SVecM.unsafeRead canv i 
        SVecM.unsafeWrite canv i $ a + polyPixel y x poly
    -}

myVec :: IO (SVec.Vector Word32)
myVec = do
    let (myBarePolys, _myNextAs) = figStepN sqrHair 14 aId
        myBarePolys' =
            map (map (\(XY x y) -> XY (x + 250) (y + 50))) myBarePolys
        myPolyBoxes = map (toIntBox . polyGetBox) myBarePolys'
        myPolys = zip myBarePolys' myPolyBoxes
    v <- SVec.unsafeThaw $ canv8Blank winW winH :: IO (SVecM.IOVector Word8)
    mapM_ (canvAdd v) myPolys
    canv8To32 <$> SVec.unsafeFreeze v

main :: IO ()
main = do
    {-
    defaultMain [
      bgroup "sqr" [bench "30" $ nf (myVec 30) 30]
      ]
    -}
    t1 <- getCurrentTime
    v <- myVec
    t2 <- getCurrentTime
    print (realToFrac $ diffUTCTime t2 t1 :: Float)
    simulateIO (InWindow "Lol" (winW, winH) (0, 0)) (greyN 0.5) 1 () (myPic v)
        (\_ _ _ -> return ())

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
    (_poly, augMs) = figStep fig a

polyArea :: ConvPoly -> Rational
polyArea poly = sum (map ptDet $ polyLines poly) / 2
  where
    ptDet (AB (XY x1 y1) (XY x2 y2)) = x1 * y2 - x2 * y1

{-
-- Return true if the point is in (or on the border of) the polygon.
isIn :: Pt -> ConvPoly -> Bool
isIn pt = all (pt .|) . polyLines
-}

pixelOutOfBox :: (Eq a, Ord a) => a -> a -> a -> a -> PolyBox a -> Bool
pixelOutOfBox !x !y !x2 !y2 !(XY (AB xMin xMax) (AB yMin yMax)) =
    if x >= xMax || y >= yMax || x2 <= xMin || y2 <= yMin then True else False

polyPixel :: Int -> Int -> ConvPoly -> Word8
polyPixel y x poly  = compute
  where
    xR = fromIntegral x
    yR = fromIntegral y
    xR2 = fromIntegral $ x + 1
    yR2 = fromIntegral $ y + 1
    isectPoly = poly `clipTo` [XY xR yR, XY xR2 yR, XY xR2 yR2, XY xR yR2]
    compute = case isectPoly of
      Nothing -> 0
      Just poly2 -> round $ 255 * abs (polyArea poly2)

onA f (AB a b) = AB (f a) b

onB f (AB a b) = AB a (f b)

-- Satisfying the first predicate prevents trying the second one.
filter2 :: (a -> Bool) -> (a -> Bool) -> [a] -> AB [a]
filter2 _ _ [] = AB [] []
filter2 p q (x:xs)
  | p x = onA (x :) $ filter2 p q xs
  | q x = onB (x :) $ filter2 p q xs
  | otherwise = filter2 p q xs

polyLineMinMax12 :: [Rational] -> AB Rational
polyLineMinMax12 [] = error "polyLineMinMax12: []"
polyLineMinMax12 [z] = AB z z
polyLineMinMax12 (z1:z2:_) = minMax z1 z2

minMax z1 z2 = if z1 <= z2 then AB z1 z2 else AB z2 z1

polyLine :: ConvPoly -> Int -> Int -> Int -> SVecM.IOVector Word8 -> Int
    -> IO ()
polyLine poly y boxX1 boxX2 canv canvI = do
    -- midPts boxX1 boxX2 canvI
    case topPts of
      [] -> case btmPts of
        [] -> midPts boxX1 boxX2
        _ -> midPts b1F b2C
      _ -> case btmPts of
        [] -> midPts t1F t2C
        _ -> if b1 <= t1
          then if b2 <= t1
            then midPts b1F t2C
            else do
              midPts b1F t1C
              fullPts t1C b2t2MinF
              midPts b2t2MinF b2t2MaxC
          else if t2 <= b1
            then midPts t1F b2C
            else do
              midPts t1F b1C
              fullPts b1C b2t2MinF
              midPts b2t2MinF b2t2MaxC
    return ()
  where
    xR = fromIntegral boxX1
    yR = fromIntegral y
    xR2 = fromIntegral boxX2
    yR2 = fromIntegral y2
    y2 = y + 1
    Just isectPoly = poly `clipTo` [XY xR yR, XY xR2 yR, XY xR2 yR2, XY xR yR2]
    AB topPts btmPts = filter2 ((== yR) . xyY) ((== yR2) . xyY) isectPoly
    AB b1 b2 = polyLineMinMax12 $ map xyX btmPts
    AB t1 t2 = polyLineMinMax12 $ map xyX topPts
    AB b2t2Min b2t2Max = minMax b2 t2
    midPts x1 x2 = doer x1 (canvI + x1)
      where
        doer x i = if x >= x2
          then return ()
          else doPt x i >> doer (x + 1) (i + 1)
    doPt x i = do
        a <- SVecM.unsafeRead canv i 
        SVecM.unsafeWrite canv i $ a + polyPixel y x poly
    fullPts x1 x2 = doer x1 (canvI + x1)
      where
        doer x i = if x >= x2
          then return ()
          else SVecM.unsafeWrite canv i 255 >> doer (x + 1) (i + 1)
    b1F = floor b1
    b1C = ceiling b1
    b2C = ceiling b2
    t1F = floor t1
    t1C = ceiling t1
    t2C = ceiling t2
    b2t2MinF = floor b2t2Min
    b2t2MaxC = ceiling b2t2Max
