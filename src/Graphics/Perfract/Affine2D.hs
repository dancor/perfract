module Graphics.Perfract.Affine2D where

import Graphics.Perfract.Pt
import Graphics.Perfract.RatRot
import Graphics.Perfract.Tupelo

data AugM = AugM
   !Rational !Rational !Rational
   !Rational !Rational !Rational

aId :: AugM
aId = AugM 1 0 0 1 0 0

-- multA ::

translateA :: Pt -> AugM -> AugM
translateA (XY tX tY) (AugM a11 a12 a21 a22 b1 b2) =
    AugM a11 a12 a21 a22 (b1 + tX) (b2 + tY)

rotateA :: RatRot -> AugM -> AugM
rotateA rr (AugM a11 a12 a21 a22 b1 b2) = AugM
    (a11 * c - a21 * s) (a12 * c - a22 * s)
    (a11 * s + a21 * c) (a12 * s + a22 * c)
    (b1 * c - b2 * s) (b1 * s + b2 * c)
  where
    c = rrCos rr
    s = rrSin rr

scaleA :: Rational -> AugM -> AugM
scaleA a (AugM a11 a12 a21 a22 b1 b2) =
    AugM (a * a11) (a * a12) (a * a21) (a * a22) (a * b1) (a * b2)

applyA :: AugM -> Pt -> Pt
applyA (AugM a11 a12 a21 a22 b1 b2) (XY x y) =
    XY (a11 * x + a12 * y + b1) (a21 * x + a22 * y + b2)
