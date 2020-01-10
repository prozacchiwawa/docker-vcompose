import Control.Exception (assert)

import qualified Data.List as List

import Util.CharPlane
import Util.Glyph
import Util.Rect

main :: IO ()
main = do
  let
    testGraph =
      [ "       +--------------------+--+"
      , "       |                    |  |"
      , " ,-----I-J--------.         |  |"
      , " | id: mainbox    |         |  |"
      , " Z Test: box      |         |  |"
      , " | Contains: foo, |  ,--.  ,Q--I---."
      , " `-----X-Y--------'  |  |  | D: M  |"
      , "       |             `__'  | id: A |"
      , "       |                   `---O---'"
      , "       +-----------------------+"
      , "    ,-------------."
      , "    |             |"
      , "    |      ,------'"
      , "    |      |"
      , "    `------'"
      ]

    testResult =
      [ Rect 1 2 18 5
      , Rect 27 5 9 4
      ]

    plane = charPlaneFromString $ List.intercalate "\n" testGraph
    cs = commas plane
    glyphs = detectGlyphs plane cs

    ts = getGlyphText plane <$> glyphs
    ps = getGlyphPorts plane <$> glyphs

    drawing = getDrawing plane

  putStrLn $ show drawing

  assert (glyphs == testResult) (putStrLn "PASS1")
