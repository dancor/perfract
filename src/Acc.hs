module Main where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as A
import qualified Data.Array.Accelerate.Interpreter as A

import Data.Word

-- Apply threshold over image using accelerate (interpreter)
thresholdAccelerate :: IO ()
thresholdAccelerate = do
  img <- either (error . show) id `fmap` A.readImageFromBMP "lol.png"
  let newImg = A.run $ A.map evalPixel (A.use img)
  A.writeImageToBMP "pumpkin-out.bmp" newImg
    where
      -- *** Exception: Prelude.Ord.compare applied to EDSL types
      evalPixel :: A.Exp A.Word32 -> A.Exp A.Word32
      evalPixel p = if p > 50 then p else 0

main :: IO ()
main = thresholdAccelerate
