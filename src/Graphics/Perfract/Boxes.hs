{-# LANGUAGE BangPatterns #-}

module Graphics.Perfract.Boxes where

import Codec.Picture
import Control.Applicative
import Control.Arrow
import Control.Concurrent.ParallelIO
import Control.DeepSeq
import Control.Monad
import Data.List
import Data.Time
import qualified Data.Vector.Mutable as MVec
import qualified Data.Vector.Storable as SVec
import qualified Data.Vector.Storable.Mutable as MSVec
import System.Environment

import Affine2D
import PolyClip
import Pt
import RatRot
import RecFig
import Tupelo

type Canv = MVec.IOVector (ABC Rational)

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
    {-
    []
    [XY 0 0, XY (-100) 200, XY 100 300, XY 300 200, XY 200 0]
    [ Prz (XY (-100) 200) (ratRot $ -0.11) (0.25)
    -- , Prz (XY 100 300) (ratRot $ -0.07)  (0.35)
    -- , Prz (XY 300 200) (ratRot $ 0.17)  (0.45)
    ]
    -}
    [ XY 0 0
    , XY (-300) 200
    , XY (-300) 500
    , XY 500 500
    , XY 500 0
    ]
    [XY 0 0, XY 0 200, XY 200 200, XY 200 0]
    [ Prz (XY 0 200) (ratRot $ -0.11) (0.55)
    , Prz (XY 200 200) (ratRot $ 0.07)  (0.55)
    ]

winW :: Int
winW = 768

winH :: Int
winH = 768

polyA :: AugM -> ConvPoly -> ConvPoly
polyA a = map (applyA a)

abcMap :: (x -> y) -> ABC x -> ABC y
abcMap f (ABC a b c) = ABC (f a) (f b) (f c)

canvAdd :: Int -> Canv -> (ConvPoly, PolyBox Int) -> IO ()
canvAdd recDepth v (poly, XY (AB x1 x2) (AB y1 y2)) =
    doLines y1 (y1 * winW)
  where
    doLines y i = if y == y2
      then return ()
      else polyLine recDepth poly y x1 x2 v i >> doLines (y + 1) (i + winW)

{-
myDraw :: Int -> Canv -> IO ()
myDraw doDepth = fromDepth 1 [aId]
  where
    fromDepth :: Int -> [AugM] -> Canv -> IO ()
    fromDepth n as v = when (n <= doDepth) $ do
        t1 <- getCurrentTime
        let (myBarePolys, nextAs) = figSteps sqrHair as
            myBarePolys' =
                map (map (\(XY x y) -> XY (x + 230) (700 - y))) myBarePolys
            myPolyBoxes = map (toIntBox . polyGetBox) myBarePolys'
            myPolys = zip myBarePolys' myPolyBoxes
        parallel_ $ map (canvAdd n v) myPolys
        t2 <- getCurrentTime
        putStrLn $ "Rendered n = " ++ show n ++ ": " ++
            show (realToFrac $ diffUTCTime t2 t1 :: Float)
        fromDepth (n + 1) nextAs v
-}

myDraw :: Int -> Canv -> IO ()
myDraw doDepth v = goParDeep 1 aId
  where
    goParDeep :: Int -> AugM -> IO ()
    goParDeep n a = do
        nextAs <- myAdd n a
        parallel_ $ if n <= 2
          then map (goParDeep (n + 1)) nextAs
          else map (goDeep (n + 1)) nextAs
    goDeep :: Int -> AugM -> IO ()
    goDeep n a = when (n <= doDepth) $ do
        nextAs <- myAdd n a
        mapM_ (goDeep (n + 1)) nextAs
    myAdd n a = do
        let (myBarePoly, nextAs) = figStep sqrHair a
            myBarePoly' =
                map (\(XY x y) -> XY (x + 230) (700 - y)) myBarePoly
            myPolyBox = toIntBox $ polyGetBox myBarePoly'
        -- parallel_ $ map (canvAdd n v) myPolys
        canvAdd n v (myBarePoly', myPolyBox)
        return nextAs

perfract :: IO ()
perfract = do
    args <- getArgs
    let (doDepth, png) = case args of
          [a1, a2] -> (read a1, a2)
          _ -> error "usage"
    v <- MVec.new (winW * winH)
    MVec.set v (ABC 0 0 0)
    do
        t1 <- getCurrentTime
        myDraw doDepth v
        t2 <- getCurrentTime
        print (realToFrac $ diffUTCTime t2 t1 :: Float)

        v2m <- MSVec.new (winW * winH * 3)
        forM_ [0 .. winW * winH - 1] $ \i -> do
            ABC r g b <- abcMap (round . (255 *)) <$> MVec.read v i
            let i3 = i * 3
                i31 = i3 + 1
                i32 = i3 + 2
            MSVec.unsafeWrite v2m i3 r
            MSVec.unsafeWrite v2m i31 g
            MSVec.unsafeWrite v2m i32 b
        v2 <- SVec.unsafeFreeze v2m
        writePng png (Image winW winH v2 :: Image PixelRGB8)
        t3 <- getCurrentTime
        print (realToFrac $ diffUTCTime t3 t2 :: Float)

doPrz :: PosRotZoom -> AugM -> AugM
doPrz (Prz p r z) = translateA p . rotateA r . scaleA z

figStep :: RecFig -> AugM -> (ConvPoly, [AugM])
figStep !fig !a = (polyA a (rPoly fig), map (flip doPrz a) $ rPrzs fig)

-- Breadth-first necessary for arbitrary-depth.
figSteps :: RecFig -> [AugM] -> ([ConvPoly], [AugM])
figSteps !fig = second concat . unzip . map (figStep fig)

{-
-- Depth-first should be fastest when doing a batch to a fixed depth.
figStepN :: RecFig -> Int -> AugM -> ([ConvPoly], [AugM])
figStepN _ 0 !a = ([], [a])
figStepN !fig 1 !a = first (:[]) (figStep fig a)
figStepN !fig !n !a =
    first (map (applyA a) (rPoly fig) :) .
    first concat . second concat . unzip $ map (figStepN fig (n - 1)) augMs
  where
    (_poly, augMs) = figStep fig a
-}

polyArea :: ConvPoly -> Rational
polyArea poly = sum (map ptDet $ polyLines poly) / 2
  where
    ptDet (AB (XY x1 y1) (XY x2 y2)) = x1 * y2 - x2 * y1

{-
pixelOutOfBox :: (Eq a, Ord a) => a -> a -> a -> a -> PolyBox a -> Bool
pixelOutOfBox !x !y !x2 !y2 !(XY (AB xMin xMax) (AB yMin yMax)) =
    if x >= xMax || y >= yMax || x2 <= xMin || y2 <= yMin then True else False
-}

polyPixel :: Int -> Int -> ConvPoly -> Rational
polyPixel y x poly  = compute
  where
    xR = fromIntegral x
    yR = fromIntegral y
    xR2 = fromIntegral $ x + 1
    yR2 = fromIntegral $ y + 1
    isectPoly = poly `clipTo` [XY xR yR, XY xR2 yR, XY xR2 yR2, XY xR yR2]
    compute = case isectPoly of
      Nothing -> 0
      Just poly2 -> abs (polyArea poly2)

-- Satisfying the first predicate prevents trying the second one.
filterABs :: (a -> Bool) -> (a -> Bool) -> [a] -> AB [a]
filterABs _ _ [] = AB [] []
filterABs p q (x:xs)
  | p x = onA (x :) $ filterABs p q xs
  | q x = onB (x :) $ filterABs p q xs
  | otherwise = filterABs p q xs

polyLineMinMax12 :: [Rational] -> AB Rational
polyLineMinMax12 [] = error "polyLineMinMax12: []"
polyLineMinMax12 [z] = AB z z
polyLineMinMax12 (z1:z2:_) = minMax z1 z2

{-
rgb :: ABC Word8 ->  PixelRGB8
rgb (ABC r g b) =  PixelRGB8 r g b
-}

minMax :: Ord a => a -> a -> AB a
minMax z1 z2 = if z1 <= z2 then AB z1 z2 else AB z2 z1

recDepthColor :: Int -> Rational -> ABC Rational
recDepthColor n val = ABC (val * (0.3 + 0.6 / nn)) (val * (1.0 - 1.0 / nn)) 0
  where
    nn = fromIntegral (n + 2) / 2
-- 1   -> 0.6 0.3
-- inf -> 0.0 1.0
--
-- f x = (a - b) / x + b
-- f 1 = a
-- f inf = b
--
--

polyLine :: Int -> ConvPoly -> Int -> Int -> Int -> Canv -> Int -> IO ()
polyLine recDepth poly y boxX1 boxX2 v vI = do
    case topPts of
      [] -> case btmPts of
        [] -> midPts boxX1 boxX2
        _ -> midPts b1F b2C
      _ -> case btmPts of
        [] -> midPts t1F t2C
        _ -> if b1 <= t1
          then if b2 <= t1
            then do
              midPts b1F t2C  -- the top abberation one
            else do
              midPts b1F t1C
              fullPts t1C b2t2MinF
              midPts b2t2MinF b2t2MaxC
          else if t2 <= b1
            then do
              -- print btmPts
              -- print topPts
              midPts t1F b2C  -- the bottom abberation one
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
    AB btmPts topPts = filterABs ((== yR) . xyY) ((== yR2) . xyY) isectPoly
    AB b1 b2 = polyLineMinMax12 $ map xyX btmPts
    AB t1 t2 = polyLineMinMax12 $ map xyX topPts
    AB b2t2Min b2t2Max = minMax b2 t2
    midPts x1 x2 = doer x1 (vI + x1)
      where
        doer x i = if x >= x2
          then return ()
          else doPt x i >> doer (x + 1) (i + 1)
    doPt x i = do
        ABC pR pG pB <- MVec.unsafeRead v i
        let ABC dR dG dB = recDepthColor recDepth (polyPixel y x poly)
        MVec.unsafeWrite v i $!!
            abcMap (min 1) $!! ABC (pR + dR) (pG + dG) (pB + dB)

    fullPts x1 x2 = doer x1 (vI + x1)
      where
        doer x i = if x >= x2
          then return ()
          else do
            MVec.unsafeWrite v i $!! recDepthColor recDepth 1
            doer (x + 1) (i + 1)
    b1F = floor b1
    b1C = ceiling b1
    b2C = ceiling b2
    t1F = floor t1
    t1C = ceiling t1
    t2C = ceiling t2
    b2t2MinF = floor b2t2Min
    b2t2MaxC = ceiling b2t2Max
