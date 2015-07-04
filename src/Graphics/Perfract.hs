{-# LANGUAGE BangPatterns #-}

module Graphics.Perfract
  ( perfract
  , module Graphics.Perfract.RatRot
  , module Graphics.Perfract.RecFig
  , module Graphics.Perfract.Tupelo
  ) where

-- Computable graphics including some kind of fractals
-- with arbitrary zoom and perfect interpolation.

-- Currenly not supporting overlapping primitives.

import Control.Concurrent.ParallelIO
import Control.DeepSeq
import Control.Monad
import Data.List
import Data.Time
import qualified Data.Vector.Mutable as MVec
import System.Environment

import Graphics.Perfract.Affine2D
import Graphics.Perfract.Canv
import Graphics.Perfract.ConvPoly
import Graphics.Perfract.PolyBox
import Graphics.Perfract.PolyClip
import Graphics.Perfract.RatRot
import Graphics.Perfract.RecFig
import Graphics.Perfract.Tupelo

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

canvAdd :: Int -> Canv -> (ConvPoly, PolyBox Int) -> IO ()
canvAdd recDepth canv@(Canv w _ _) (poly, XY (AB x1 x2) (AB y1 y2)) =
    doLines y1 (y1 * w)
  where
    doLines y i = if y == y2
      then return ()
      else polyLine recDepth poly y x1 x2 canv i >> doLines (y + 1) (i + w)

polyA :: AugM -> ConvPoly -> ConvPoly
polyA a = map (applyA a)

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

drawFig :: Int -> Canv -> RecFig -> IO ()
drawFig doDepth v myFig = goParDeep 1 aId
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
        let (myBarePoly, nextAs) = figStep myFig a
            myBarePoly' =
                map (\(XY x y) -> XY (x + 230) (700 - y)) myBarePoly
            myPolyBox = toIntBox $ polyGetBox myBarePoly'
        -- parallel_ $ map (canvAdd n v) myPolys
        canvAdd n v (myBarePoly', myPolyBox)
        return nextAs

perfract :: Int -> Int -> RecFig -> IO ()
perfract w h myFig = do
    args <- getArgs
    let (doDepth, pngF) = case args of
          [a1, a2] -> (read a1, a2)
          _ -> (8, "out.png") -- error "usage"
    v <- newCanv w h
    do
        t1 <- getCurrentTime
        drawFig doDepth v myFig
        t2 <- getCurrentTime
        print $ diffUTCTime t2 t1

        saveCanv pngF v
        t3 <- getCurrentTime
        print $ diffUTCTime t3 t2

doPrz :: PosRotZoom -> AugM -> AugM
doPrz (Prz p r z) = translateA p . rotateA r . scaleA z

figStep :: RecFig -> AugM -> (ConvPoly, [AugM])
figStep !fig !a = (polyA a (rPoly fig), map (flip doPrz a) $ rPrzs fig)

-- Breadth-first necessary for arbitrary-depth.
--figSteps :: RecFig -> [AugM] -> ([ConvPoly], [AugM])
--figSteps !fig = second concat . unzip . map (figStep fig)

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
