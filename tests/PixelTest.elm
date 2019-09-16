module PixelTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange)
import Test exposing (..)

import PixelFuzz exposing (..)


import List.Extra exposing (groupsOf, zip)

import Array
import Pixel exposing (Pixel)
import PixelInfo exposing (Mode(..))


suite : Test
suite =
  describe "Pixel"
    [ pixelToListAndBack
    , getColors
    ]


pixelToListAndBack =
  fuzz rgbaFuzzer "rgba pixel from list" <| \list ->
      Pixel.fromList list
        |> Pixel.toList
        |> Expect.equal list


getColors =
  let
      fuzzer =
        Fuzz.map2 Tuple.pair colorRangeFuzzer colorRangeFuzzer
  in
  describe "get colors from pixel"
    [ fuzz fuzzer "get red from pixel" <| \(int, int2) ->
      Pixel.fromList [ int, int2, int2, int2 ]
        |> Pixel.red
        |> Expect.equal int

    , fuzz fuzzer "get green from pixel" <| \(int, int2) ->
      Pixel.fromList [ int2, int, int2, int2 ]
        |> Pixel.green
        |> Expect.equal int

    , fuzz fuzzer "get blue from pixel" <| \(int, int2) ->
      Pixel.fromList [ int2, int2, int, int2 ]
        |> Pixel.blue
        |> Expect.equal int

    , fuzz fuzzer "get alpha from pixel" <| \(int, int2) ->
      Pixel.fromList [ int2, int2, int2, int ]
        |> Pixel.alpha
        |> Expect.equal int
    ]


