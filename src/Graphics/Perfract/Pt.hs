module Graphics.Perfract.Pt where

import Graphics.Perfract.Tupelo

type Pt a = XY a

type Pt3 = XYZ Rational

type Ln a = AB (Pt a)
