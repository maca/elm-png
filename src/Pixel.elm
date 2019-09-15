module Pixel exposing (Pixel, blank, group)


import Array exposing (Array)
import List.Extra exposing (groupsOf)


import PixelInfo exposing (PixelInfo, channels, bitDepth)


type alias Pixel
  = List Int


blank : Pixel
blank = [0, 0, 0]


group : PixelInfo -> List Int -> Array Pixel
group pixelInfo ln =
  groupsOf 3 ln |> Array.fromList
