{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Graphics.Perfract.Tupelo

show3 :: Show x => ABC x -> BS.ByteString
show3 (ABC a b c) = BS.pack (show a)
  <> BS.cons ' ' (BS.pack $ show b)
  <> BS.cons ' ' (BS.pack $ show c)

show4 :: Show x => ABCD x -> BS.ByteString
show4 (ABCD a b c d) = BS.pack (show a)
  <> BS.cons ' ' (BS.pack $ show b)
  <> BS.cons ' ' (BS.pack $ show c)
  <> BS.cons ' ' (BS.pack $ show d)

showV :: ABC Double -> BS.ByteString
showV = ("v " <>) . show3

showF4 :: ABCD Int -> BS.ByteString
showF4 = ("f " <>) . show4

qInc :: Int -> ABCD Int -> ABCD Int
qInc i (ABCD a b c d) = ABCD (a + i) (b + i) (c + i) (d + i)

doCube :: Int -> Double -> ABC Double -> [BS.ByteString]
doCube i scale (ABC oX oY oZ) =
    [ showV (ABC x1 y1 z1)
    , showV (ABC x1 y1 z2)
    , showV (ABC x1 y2 z2)
    , showV (ABC x1 y2 z1)
    , showV (ABC x2 y1 z1)
    , showV (ABC x2 y1 z2)
    , showV (ABC x2 y2 z2)
    , showV (ABC x2 y2 z1)
    ] ++ map (showF4 . qInc i)
    [ ABCD 1 2 3 4
    , ABCD 5 8 7 6
    , ABCD 1 5 6 2
    , ABCD 2 6 7 3
    , ABCD 3 7 8 4
    , ABCD 5 1 4 8
    ]
  where
    x1 = -1 * scale + oX
    x2 = 1 * scale + oX
    y1 = -1 * scale + oY
    y2 = 1 * scale + oY
    z1 = -1 * scale + oZ
    z2 = 1 * scale + oZ

doCubeRec :: Int -> Double -> ABC Double -> [BS.ByteString]
doCubeRec 1 i scale offset = doCube i scale
doCubeRec d i scale offset =
    doCube i scale ++
    doCubeRec (d - 1)

main :: IO ()
main = do
    mapM_ BS.putStrLn $ doCubeRec 2 0 1 (ABC 0 0 0)
