module Graphics.Perfract.Canv where

import Graphics.Perfract.Tupelo
import qualified Data.Vector.Mutable as MVec

type Canv = MVec.IOVector (ABC Rational)
