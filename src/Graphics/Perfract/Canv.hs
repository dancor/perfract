{-# LANGUAGE FlexibleInstances #-}

-- Rational canvas with showing to screen and saving to file.

module Graphics.Perfract.Canv where

import Codec.Picture
import Control.Applicative
import Control.Monad
import Data.Ratio
import Data.Word

import Graphics.Perfract.Tupelo
import qualified Data.Vector.Mutable as MVec
import qualified Data.Vector.Storable as SVec
import qualified Data.Vector.Storable.Mutable as MSVec

-- This represents a pixel channel (eg. R, G, or B) during manipulation,
-- where higher working precision (than the final 0-255) allows more accuracy.
class DeepSubpixel a where
    toSubpixel :: a -> Word8

instance DeepSubpixel Float where
    toSubpixel = round . (255 *)

instance DeepSubpixel (Ratio Integer) where
    toSubpixel = round . (255 *)

data Canv a = Canv
    { cW :: !Int
    , cH :: !Int
    , cV :: MVec.IOVector (ABC a)
    }

newCanv :: DeepSubpixel a => Int -> Int -> IO (Canv a)
newCanv w h = do
    v <- MVec.new (w * h)
    MVec.set v (ABC 0 0 0)
    return $ Canv w h v

saveCanv :: DeepSubpixel a => FilePath -> Canv a -> IO ()
saveCanv pngF (Canv w h v) = do
    v2m <- MSVec.new (w * h * 3)
    forM_ [0 .. w * h - 1] $ \i -> do
        ABC r g b <- abcMap toSubpixel <$> MVec.read v i
        let i3 = i * 3
            i31 = i3 + 1
            i32 = i3 + 2
        MSVec.unsafeWrite v2m i3 r
        MSVec.unsafeWrite v2m i31 g
        MSVec.unsafeWrite v2m i32 b
    v2 <- SVec.unsafeFreeze v2m
    writePng pngF (Image w h v2 :: Image PixelRGB8)

--canvGo :: Canv -> IO ()
--canvGo 
