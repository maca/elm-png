module PixelFuzz exposing (..)


import Fuzz exposing (Fuzzer, constant, int, intRange)


rgbFuzzer : Fuzzer (List Int)
rgbFuzzer =
  Fuzz.map3 (\r g b -> [ r, g, b ])
    colorRangeFuzzer
    colorRangeFuzzer
    colorRangeFuzzer


rgbaFuzzer : Fuzzer (List Int)
rgbaFuzzer =
  Fuzz.map4 (\r g b a -> [ r, g, b, a ])
    colorRangeFuzzer
    colorRangeFuzzer
    colorRangeFuzzer
    colorRangeFuzzer


grayAFuzzer : Fuzzer (Int, Int)
grayAFuzzer =
  Fuzz.map2 (\i a -> ( i, a ))
    colorRangeFuzzer
    colorRangeFuzzer


rgbListFuzzer : Fuzzer (List (List Int))
rgbListFuzzer =
  Fuzz.map3 (\p1 p2 p3 -> [ p1, p2, p3 ])
    rgbFuzzer
    rgbFuzzer
    rgbFuzzer


rgbaListFuzzer : Fuzzer (List (List Int))
rgbaListFuzzer =
  Fuzz.map3 (\p1 p2 p3 -> [ p1, p2, p3 ])
    rgbaFuzzer
    rgbaFuzzer
    rgbaFuzzer


grayAListFuzzer : Fuzzer (List (Int, Int))
grayAListFuzzer =
  Fuzz.map2 (\p1 p2 -> [ p1, p2 ])
    grayAFuzzer
    grayAFuzzer


colorRangeFuzzer : Fuzzer Int
colorRangeFuzzer =
  intRange 0 255
