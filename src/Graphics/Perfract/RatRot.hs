-- Approximate rational-degree rotations with irrational-degree
-- rotations a that have rational sin a and cos a.
-- This approximation can be made arbitrarily close.
module Graphics.Perfract.RatRot (RatRot, ratRot, rrSin, rrCos) where

import Data.Ratio

data RatRot = RatRot
    { rrSin :: !Rational
    , rrCos :: !Rational
    } deriving Show

ratRot :: Rational -> RatRot
ratRot revsCW
  | revsCW <= -1/2 = error "ratRot revsCW should have revsCW > -1/2"
  | revsCW > 1/2 = error "ratRot revsCW should have revsCW <= -1/2"
  | otherwise = RatRot sinThetaApprox cosThetaApprox where
    theta = -2 * pi * fromRational revsCW :: Double
    yOverX = tan theta
    nOverMApprox = yOverX + sqrt (1 + yOverX * yOverX)
    m = 100
    n = round (nOverMApprox * fromIntegral m)
    nSq = n * n
    mSq = m * m
    denom = nSq + mSq
    sinThetaApprox = (nSq - mSq) % denom
    cosThetaApprox = (2 * n * m) % denom
