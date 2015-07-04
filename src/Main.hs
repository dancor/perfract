import Graphics.Perfract

--  . . . .
-- . o   o  .
-- .o/\  /\o.
--   \/__\/
--    |  |
--    |__|
sqrHair :: RecFig
sqrHair = RecFig
    {-
    []
    [XY 0 0, XY (-100) 200, XY 100 300, XY 300 200, XY 200 0]
    [ Prz (XY (-100) 200) (ratRot $ -0.11) (0.25)
    -- , Prz (XY 100 300) (ratRot $ -0.07)  (0.35)
    -- , Prz (XY 300 200) (ratRot $ 0.17)  (0.45)
    ]
    -}
    [ XY 0 0
    , XY (-300) 200
    , XY (-300) 500
    , XY 500 500
    , XY 500 0
    ]
    [XY 0 0, XY 0 200, XY 200 200, XY 200 0]
    [ Prz (XY 0 200) (ratRot $ -0.11) (0.55)
    , Prz (XY 200 200) (ratRot $ 0.07)  (0.55)
    ]

main :: IO ()
main = perfract 768 768 sqrHair
