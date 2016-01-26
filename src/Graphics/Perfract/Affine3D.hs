module Graphics.Perfract.Affine3D where

import Graphics.Perfract.Pt
import Graphics.Perfract.RatRot
import Graphics.Perfract.Tupelo

data Aug3M = Aug3M
   !Rational !Rational !Rational
   !Rational !Rational !Rational
   !Rational !Rational !Rational
   !Rational !Rational !Rational

a3Id :: Aug3M
a3Id = Aug3M
    1 0 0
    0 1 0
    0 0 1
    0 0 0

translateA3 :: Pt3 -> Aug3M -> Aug3M
translateA3 (XYZ tX tY tZ) (Aug3M a11 a12 a13 a21 a22 a23 a31 a32 a33 b1 b2 b3)
    = Aug3M a11 a12 a13 a21 a22 a23 a31 a32 a33 (b1 + tX) (b2 + tY) (b3 + tZ)

xRotateA3 :: RatRot -> Aug3M -> Aug3M
xRotateA3 rr (Aug3M
        a11 a12 a13
        a21 a22 a23
        a31 a32 a33
        b1 b2 b3) =
    Aug3M
    a11 a12 a13
    (a21 * c - a31 * s) (a22 * c - a32 * s) (a23 * c - a33 * s)
    (a21 * s + a31 * c) (a22 * s + a32 * c) (a23 * s + a33 * c)
    b1 (b2 * c - b3 * s) (b2 * s + b3 * c)
  where
    c = rrCos rr
    s = rrSin rr

{-
xrotA3 :: RatRot -> Aug3M -> Aug3M
xrotA3 rr (Aug3M a11 a12 a13 a21 a22 a23 a31 a32 a33 b1 b2 b3) = Aug3M
    (a11 * c - a21 * s) (a12 * c - a22 * s)
    (a11 * s + a21 * c) (a12 * s + a22 * c)
    (b1 * c - b2 * s) (b1 * s + b2 * c)
  where
    c = rrCos rr
    s = rrSin rr

xrotA3 :: RatRot -> Aug3M -> Aug3M
xrotA3 rr (Aug3M a11 a12 a13 a21 a22 a23 a31 a32 a33 b1 b2 b3) = Aug3M
    (a11 * c - a21 * s) (a12 * c - a22 * s)
    (a11 * s + a21 * c) (a12 * s + a22 * c)
    (b1 * c - b2 * s) (b1 * s + b2 * c)
  where
    c = rrCos rr
    s = rrSin rr
-}

scaleA3 :: Rational -> Aug3M -> Aug3M
scaleA3 a (Aug3M a11 a12 a13 a21 a22 a23 a31 a32 a33 b1 b2 b3) = Aug3M
    (a * a11) (a * a12) (a * a13)
    (a * a21) (a * a22) (a * a23)
    (a * a31) (a * a32) (a * a33)
    (a * b1) (a * b2) (a * b3)

applyA3 :: Aug3M -> Pt3 -> Pt3
applyA3 (Aug3M a11 a12 a13 a21 a22 a23 a31 a32 a33 b1 b2 b3) (XYZ x y z) = XYZ
    (a11 * x + a12 * y + a13 * z + b1)
    (a21 * x + a22 * y + a23 * z + b2)
    (a31 * x + a32 * y + a33 * z + b3)
