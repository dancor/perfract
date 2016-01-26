module Graphics.Perfract.Shape where

import qualified Data.Vector as Vec

import Graphics.Perfract.Tupelo

data Shape = Shape
    { shPts :: !(Vec.Vector (XYZ Rational))
    , shQuads :: !(Vec.Vector (ABCD Int))
    }

basicCube :: Shape
basicCube = Shape
    (Vec.fromList 
        [ XYZ (-1) (-1) (-1)
        , XYZ (-1) (-1)   1
        , XYZ (-1)   1    1 
        , XYZ (-1)   1  (-1)
        , XYZ   1  (-1) (-1)
        , XYZ   1  (-1)   1
        , XYZ   1    1    1 
        , XYZ   1    1  (-1)
        ]
    )
    (Vec.fromList
        [ ABCD 0 1 2 3
        , ABCD 4 7 6 5
        , ABCD 0 4 5 1
        , ABCD 1 5 6 2
        , ABCD 2 6 7 3
        , ABCD 4 0 3 7
        ]
    )   
