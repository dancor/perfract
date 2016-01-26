{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Perfract
  ( perfract3
  , module Graphics.Perfract.RatRot
  , module Graphics.Perfract.RecFig
  , module Graphics.Perfract.RecFig3
  , module Graphics.Perfract.Tupelo
  ) where

-- Computable graphics including some kind of fractals
-- with arbitrary zoom and perfect interpolation.

-- Currenly not supporting overlapping primitives.

--import Control.Concurrent.ParallelIO
import Control.DeepSeq
import Control.Monad
import Data.List
import Data.Monoid
import Data.Time
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import System.Environment

import qualified Data.ByteString.Char8 as BS

import Graphics.Perfract.Affine2D
import Graphics.Perfract.Affine3D
import Graphics.Perfract.Canv
import Graphics.Perfract.ConvPoly
import Graphics.Perfract.PolyBox
import Graphics.Perfract.PolyClip
import Graphics.Perfract.Pt
import Graphics.Perfract.RatRot
import Graphics.Perfract.RecFig
import Graphics.Perfract.RecFig3
import Graphics.Perfract.Shape
import Graphics.Perfract.Tupelo

perfract3 :: RecFig3 -> IO ()
perfract3 fig = do
    let doDepth = 12
    --t1 <- getCurrentTime
    _ <- drawFig3 doDepth fig 1 a3Id
    --t2 <- getCurrentTime
    --putStrLn $ "Time: " ++ show (diffUTCTime t2 t1)
    return ()

{-
perfract :: Int -> Int -> RecFig -> IO ()
perfract w h fig = do
    args <- getArgs
    let (doDepth, pngF) = case args of
          [a1, a2] -> (read a1, a2)
          _ -> (888, "out.png") -- error "usage"
    v <- newCanv w h
    do
        t1 <- getCurrentTime
        drawFig doDepth v fig aId
        t2 <- getCurrentTime
        putStrLn $ "Draw: " ++ show (diffUTCTime t2 t1)

        saveCanv pngF v
        t3 <- getCurrentTime
        putStrLn $ "Save: " ++ show (diffUTCTime t3 t2)

toIntBox :: PolyBox Rational -> PolyBox Int
toIntBox !(XY (AB x1 x2) (AB y1 y2)) =
    XY (AB (floor x1) (ceiling x2)) (AB (floor y1) (ceiling y2))

polyGetBox :: ConvPoly -> PolyBox Rational
polyGetBox = polyGetBoxL . Vec.toList

polyGetBoxL :: [Pt] -> PolyBox Rational
polyGetBoxL (XY x1 y1 : XY x2 y2 : rest) = foldl' trav (XY x12 y12) rest
  where
    x12 = if x1 <= x2 then AB x1 x2 else AB x2 x1
    y12 = if y1 <= y2 then AB y1 y2 else AB y2 y1
    trav !(XY xs ys) !(XY x y) = XY (incl x xs) (incl y ys)
    incl !z !a@(AB zMin zMax) =
      if z <= zMin then AB z zMax else
      if z >= zMax then AB zMin z else a
polyGetBoxL _ = error "polyGetBox: invalid poly"

canvAdd :: Int -> Canv -> (ConvPoly, PolyBox Int) -> IO ()
canvAdd recDepth canv@(Canv w _ _) (poly, XY (AB x1 x2) (AB y1 y2)) =
    doLines y1 (y1 * w)
  where
    doLines y i = when (y /= y2) $
        polyLine recDepth poly y x1 x2 canv i >> doLines (y + 1) (i + w)

polyA :: AugM -> ConvPoly -> ConvPoly
polyA a = Vec.map (applyA a)

drawFig :: Int -> Canv -> RecFig -> AugM -> IO ()
drawFig 0 _ _ _ = return ()
drawFig !doDepth !v !fig !augM = do
    let (barePolyPreT, nextAugMs) = figStep fig augM
        barePoly = Vec.map (\(XY x y) -> XY (x + 230) (700 - y)) barePolyPreT
        polyBox = toIntBox $ polyGetBox barePoly
    canvAdd doDepth v (barePoly, polyBox)
    drawFig (doDepth - 1) v fig (Vec.head nextAugMs)

doPrz :: PosRotZoom -> AugM -> AugM
doPrz (Prz p r z) = translateA p . rotateA r . scaleA z

figStep :: RecFig -> AugM -> (ConvPoly, Vec.Vector AugM)
figStep !fig !a = (polyA a (rPoly fig), Vec.map (flip doPrz a) $ rPrzs fig)
-}

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

{-
pixelOutOfBox :: (Eq a, Ord a) => a -> a -> a -> a -> PolyBox a -> Bool
pixelOutOfBox !x !y !x2 !y2 !(XY (AB xMin xMax) (AB yMin yMax)) =
    if x >= xMax || y >= yMax || x2 <= xMin || y2 <= yMin then True else False

polyPixel :: Int -> Int -> ConvPoly -> Rational
polyPixel y x poly  = compute
  where
    xR = fromIntegral x
    yR = fromIntegral y
    xR2 = fromIntegral $ x + 1
    yR2 = fromIntegral $ y + 1
    isectPoly = poly `clipTo`
        Vec.fromList [XY xR yR, XY xR2 yR, XY xR2 yR2, XY xR yR2]
    compute = case isectPoly of
      Nothing -> 0
      Just poly2 -> abs (polyArea poly2)
-}

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

{-
polyLine :: Int -> ConvPoly -> Int -> Int -> Int -> Canv -> Int -> IO ()
polyLine recDepth poly y boxX1 boxX2 (Canv _ _ v) vI = do
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
    Just isectPoly = poly `clipTo`
        Vec.fromList [XY xR yR, XY xR2 yR, XY xR2 yR2, XY xR yR2]
    AB btmPts topPts = filterABs ((== yR) . xyY) ((== yR2) . xyY) $
        Vec.toList isectPoly
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
-}

doPrz3 :: PosRotZoom3 -> Aug3M -> Aug3M
doPrz3 (Prz3 p r z) = translateA3 p . xRotateA3 r . scaleA3 z

ratToDub :: Rational -> Double
ratToDub x = realToFrac x

showPt :: XYZ Rational -> IO ()
showPt (XYZ x y z) = do
    let f = BS.pack . show . ratToDub
    BS.putStrLn $ "v " <> f x <> " " <> f y <> " " <> f z

showQuadInc :: Int -> ABCD Int -> IO ()
showQuadInc i (ABCD a b c d) = do
    let f = BS.pack . show . (i +)
    BS.putStrLn $ "f " <> f a <> " " <> f b <> " " <> f c <> " " <> f d

showShapeAt :: Shape -> Int -> Aug3M -> IO Int
showShapeAt (Shape pts quads) nextPtsI a = do
    Vec.mapM_ (showPt . applyA3 a) pts
    Vec.mapM_ (showQuadInc nextPtsI) quads
    return $ nextPtsI + Vec.length pts

drawFig3 :: Int -> RecFig3 -> Int -> Aug3M -> IO Int
drawFig3 1 !(RecFig3 shape _) !nextPtI !aug3M = showShapeAt shape nextPtI aug3M
drawFig3 !doDepth !fig@(RecFig3 shape prz3s) !nextPtI !aug3M = do
    nextPtI2 <- showShapeAt shape nextPtI aug3M
    let nextAug3Ms = map (flip doPrz3 aug3M) $ Vec.toList prz3s
    foldM (drawFig3 (doDepth - 1) fig) nextPtI2 nextAug3Ms
