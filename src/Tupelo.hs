-- Some strict tuples for grouping data as you like it.

module Tupelo where

import Control.Applicative
import Control.DeepSeq
import Foreign
import Foreign.Storable.Record as Store

data XY a = XY
    { xyX :: !a
    , xyY :: !a
    } deriving Show

instance NFData a => NFData (XY a) where
    rnf (XY x y) = x `seq` y `seq` ()

storeXY :: Storable a => Store.Dictionary (XY a)
storeXY = Store.run $ liftA2 XY (Store.element xyX) (Store.element xyY)

instance Storable a => Storable (XY a) where
    sizeOf = Store.sizeOf storeXY
    alignment = Store.alignment storeXY
    peek = Store.peek storeXY
    poke = Store.poke storeXY

-- "X, Y, width, height"
data XYWH a = XYWH
    { xywhX :: !a
    , xywhY :: !a
    , xywhW :: !a
    , xywhH :: !a
    } deriving Show

data AB a = AB
    { abA :: !a
    , abB :: !a
    } deriving Show

storeAB :: Storable a => Store.Dictionary (AB a)
storeAB = Store.run $ liftA2 AB (Store.element abA) (Store.element abB)

instance Storable a => Storable (AB a) where
    sizeOf = Store.sizeOf storeAB
    alignment = Store.alignment storeAB
    peek = Store.peek storeAB
    poke = Store.poke storeAB

data ABC a = ABC
    { abcA :: !a
    , abcB :: !a
    , abcC :: !a
    } deriving Show
