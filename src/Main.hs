import qualified Data.Vector as Vec

import Graphics.Perfract

--  . . . .
-- . o   o  .
-- .o/\  /\o.
--   \/__\/
--    |  |
--    |__|
skyTri :: RecFig
skyTri = RecFig
    {-
    []
    [XY 0 0, XY (-100) 200, XY 100 300, XY 300 200, XY 200 0]
    [ Prz (XY (-100) 200) (ratRot $ -0.11) (0.25)
    -- , Prz (XY 100 300) (ratRot $ -0.07)  (0.35)
    -- , Prz (XY 300 200) (ratRot $ 0.17)  (0.45)
    ]
    -}
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
main = perfract 300 300 skyTri
