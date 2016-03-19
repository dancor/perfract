import qualified Data.Vector as Vec

import Data.Time
import System.Environment

import Graphics.Perfract

skyTri :: RecFig
skyTri = RecFig
    (Vec.fromList
        [ XY 0 0
        , XY (-300) 200
        , XY (-300) 500
        , XY 500 500
        , XY 500 0
        ])
    (Vec.fromList [XY 0 0, XY 0.2 0.4, XY (-0.2) 0.4])
    (Vec.fromList
        [ Prz (XY (-0.2) 0.4) (ratRot $ -0.11) (0.55)
        , Prz (XY 0.2 0.4) (ratRot $ 0.07)  (0.55)
        ])

main :: IO ()
main = do
    [nStr] <- getArgs

    v <- newCanv 600 600
    t1 <- getCurrentTime
    perfract (read nStr) v skyTri
    t2 <- getCurrentTime
    putStrLn $ "Draw: " ++ show (diffUTCTime t2 t1)

    saveCanv "out.png" v
    t3 <- getCurrentTime
    putStrLn $ "Save: " ++ show (diffUTCTime t3 t2)
