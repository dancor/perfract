{-# LANGUAGE BangPatterns #-}

-- Computable graphics including some kind of fractals
-- with arbitrary zoom and perfect interpolation.

-- Currenly not supporting overlapping primitives.

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.ParallelIO
import Control.Monad
import Data.Bits
import Data.List
import Data.Time
import Data.Word
import qualified Data.Vector.Storable as SVec
import qualified Data.Vector.Storable.Mutable as SMVec
import Data.Vector.Storable.ByteString
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate

import Affine2D
import PolyClip
import Pt
import RatRot
import RecFig
import Tupelo

type Canv = SMVec.IOVector Word32

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

myModelToPic :: Canv -> () -> IO Picture
myModelToPic v () = do
    v2 <- SVec.freeze v
    return $ bitmapOfByteString winW winH (vectorToByteString v2) True

{-
canvTo32 :: SVec.Vector Word24 -> SVec.Vector Word32
canvTo32 = SVec.map f
  where
    f x = ((r `shift` 8 .|. g) `shift` 8 .|. b) `shift` 8 .|. a
      where
        r = 0
        g = fromIntegral x
        b = 0
        a = 255
-}

canvAdd :: Canv -> (ConvPoly, PolyBox Int) -> IO ()
canvAdd v (poly, XY (AB x1 x2) (AB y1 y2)) =
    doLines y1 (y1 * winW)
  where
    doLines y i = if y == y2
      then return ()
      else polyLine poly y x1 x2 v i >> doLines (y + 1) (i + winW)

myDraw :: Canv -> IO ()
myDraw v = myDrawLoop (1 :: Int) [aId] where
    myDrawLoop n as = when (n <= 10000) $ do
        let (myBarePolys, nextAs) = figSteps sqrHair as
            myBarePolys' =
                map (map (\(XY x y) -> XY (x + 250) (y + 50))) myBarePolys
            myPolyBoxes = map (toIntBox . polyGetBox) myBarePolys'
            myPolys = zip myBarePolys' myPolyBoxes
        -- mapM_ (canvAdd v) myPolys
        parallel_ $ map (canvAdd v) myPolys
        myDrawLoop (n + 1) nextAs

main :: IO ()
main = do
    v <- SMVec.new (winW * winH)
    SMVec.set v 0
    let displayMode = InWindow "Lol" (winW, winH) (0, 0)
        simStepsPerSec = 1 :: Int
        showIt = True
    if showIt
      then do
        _ <- forkIO (myDraw v)
        simulateIO displayMode black simStepsPerSec ()
            (myModelToPic v) (\_ _ _ -> return ())
      else do
        t1 <- getCurrentTime
        myDraw v
        t2 <- getCurrentTime
        print (realToFrac $ diffUTCTime t2 t1 :: Float)

doPrz :: PosRotZoom -> AugM -> AugM
doPrz (Prz p r z) = translateA p . rotateA r . scaleA z

figStep :: RecFig -> AugM -> (ConvPoly, [AugM])
figStep !fig !a = (polyA a (rPoly fig), map (flip doPrz a) $ rPrzs fig)

-- Breadth-first necessary for arbitrary-depth.
figSteps :: RecFig -> [AugM] -> ([ConvPoly], [AugM])
figSteps !fig = second concat . unzip . map (figStep fig)

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
pixelOutOfBox :: (Eq a, Ord a) => a -> a -> a -> a -> PolyBox a -> Bool
pixelOutOfBox !x !y !x2 !y2 !(XY (AB xMin xMax) (AB yMin yMax)) =
    if x >= xMax || y >= yMax || x2 <= xMin || y2 <= yMin then True else False
-}

polyPixel :: Int -> Int -> ConvPoly -> Word32
polyPixel y x poly  = compute
  where
    xR = fromIntegral x
    yR = fromIntegral y
    xR2 = fromIntegral $ x + 1
    yR2 = fromIntegral $ y + 1
    isectPoly = poly `clipTo` [XY xR yR, XY xR2 yR, XY xR2 yR2, XY xR yR2]
    compute = case isectPoly of
      Nothing -> 0
      Just poly2 -> rgb (round $ 255 * abs (polyArea poly2)) 0 0

polyPixel2 :: Int -> Int -> ConvPoly -> Word32
polyPixel2 y x poly  = compute
  where
    xR = fromIntegral x
    yR = fromIntegral y
    xR2 = fromIntegral $ x + 1
    yR2 = fromIntegral $ y + 1
    isectPoly = poly `clipTo` [XY xR yR, XY xR2 yR, XY xR2 yR2, XY xR yR2]
    compute = case isectPoly of
      Nothing -> 0
      Just poly2 -> rgb 0 0 (round $ 255 * abs (polyArea poly2))

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

rgb :: Word8 -> Word8 -> Word8 -> Word32
rgb r g b =
    ((rr `shift` 8 .|. gg) `shift` 8 .|. bb) `shift` 8 .|. 255
  where
    rr = fromIntegral r
    gg = fromIntegral g
    bb = fromIntegral b

minMax :: Ord a => a -> a -> AB a
minMax z1 z2 = if z1 <= z2 then AB z1 z2 else AB z2 z1

polyLine :: ConvPoly -> Int -> Int -> Int -> Canv -> Int -> IO ()
polyLine poly y boxX1 boxX2 canv canvI = do
    case topPts of
      [] -> case btmPts of
        [] -> midPts boxX1 boxX2
        _ -> midPts b1F b2C
      _ -> case btmPts of
        [] -> midPts t1F t2C
        _ -> if b1 <= t1
          then if b2 <= t1
            then do
              midPts2 b1F t2C  -- the top abberation one
            else do
              midPts b1F t1C
              fullPts t1C b2t2MinF
              midPts b2t2MinF b2t2MaxC
          else if t2 <= b1
            then do
              print btmPts
              print topPts
              midPts2 t1F b2C  -- the bottom abberation one
            else do
              midPts t1F b1C
              fullPts b1C b2t2MinF
              midPts b2t2MinF b2t2MaxC
  where
    xR = fromIntegral boxX1
    yR = fromIntegral y
    xR2 = fromIntegral boxX2
    yR2 = fromIntegral y2
    y2 = y + 1
    Just isectPoly = poly `clipTo` [XY xR yR, XY xR2 yR, XY xR2 yR2, XY xR yR2]
    AB btmPts topPts = filter2 ((== yR) . xyY) ((== yR2) . xyY) isectPoly
    AB b1 b2 = polyLineMinMax12 $ map xyX btmPts
    AB t1 t2 = polyLineMinMax12 $ map xyX topPts
    AB b2t2Min b2t2Max = minMax b2 t2
    midPts x1 x2 = doer x1 (canvI + x1)
      where
        doer x i = if x >= x2
          then return ()
          else doPt x i >> doer (x + 1) (i + 1)
    doPt x i = do
        a <- SMVec.unsafeRead canv i
        SMVec.unsafeWrite canv i $ a + polyPixel y x poly

    midPts2 x1 x2 = doer x1 (canvI + x1)
      where
        doer x i = if x >= x2
          then return ()
          else doPt2 x i >> doer (x + 1) (i + 1)
    doPt2 x i = do
        a <- SMVec.unsafeRead canv i
        SMVec.unsafeWrite canv i $ a + polyPixel2 y x poly

    fullPts x1 x2 = doer x1 (canvI + x1)
      where
        doer x i = if x >= x2
          then return ()
          else SMVec.unsafeWrite canv i 0xffffffff >> doer (x + 1) (i + 1)
    b1F = floor b1
    b1C = ceiling b1
    b2C = ceiling b2
    t1F = floor t1
    t1C = ceiling t1
    t2C = ceiling t2
    b2t2MinF = floor b2t2Min
    b2t2MaxC = ceiling b2t2Max
