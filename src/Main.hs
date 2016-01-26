import qualified Data.Vector as Vec

import Graphics.Perfract
import Graphics.Perfract.Shape

--  . . . .
-- . o   o  .
-- .o/\  /\o.
--   \/__\/
--    |  |
--    |__|
sqrHair :: RecFig3
sqrHair = RecFig3 basicCube
    (Vec.fromList
        [ Prz3 (XYZ 1.6 0.4 0.4) (ratRot $ 0.1) (0.6)
        , Prz3 (XYZ 1.6 (-0.4) (-0.4)) (ratRot $ 0.2) (0.6)
        ])

main :: IO ()
main = perfract3 sqrHair
