import Control.Exception (assert)

import qualified Data.List as List

import Util.CharPlane
import Util.Glyph
import Util.Rect

main :: IO ()
main = do
  let
    testGraph =
      [ ""
      , " ,----------------."
      , " | Test: box      |"
      , " | Contains: foo, |  ,--.  ,-------."
      , " `----------------'  |  |  | D: M  |"
      , "                     `--'  `-------'"
      , "    ,-------------."
      , "    |             |"
      , "    |      ,------'"
      , "    |      |"
      , "    `------'"
      ]

    testResult =
      [ Rect 1 1 18 4
      , Rect 27 3 9 3
      ]

    plane = charPlaneFromString $ List.intercalate "\n" testGraph
    cs = commas plane
    glyphs = detectGlyphs plane cs

    ts = getGlyphText plane <$> glyphs

  putStrLn $ show $ detectGlyph plane (27,3)
  putStrLn $ show cs
  putStrLn $ show glyphs
  putStrLn $ show ts

  assert (glyphs == testResult) (putStrLn "PASS1")
