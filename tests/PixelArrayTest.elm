module PixelArrayTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange)
import Test exposing (..)

import PixelFuzz exposing (..)


import List.Extra exposing (groupsOf, zip)

import Array
import Pixel exposing (Pixel)
import PixelInfo exposing (Mode(..))
import PixelArray


suite : Test
suite =
  describe "Pixel"
    [ makeRGBPixelArray
    , makeRGBAPixelArray
    , makeGrayscalePixelArray
    , makeGrayscaleAlphaPixelArray
    ]


makeRGBPixelArray =
  let
      fuzzer =
        Fuzz.map2 Tuple.pair rgbListFuzzer colorRangeFuzzer

      expected list =
        List.map (\p -> Pixel.fromList <| p ++ [255]) list
          |> Array.fromList
  in
  describe "make RGB pixels"
    [ fuzz fuzzer "makes pixels from 8 bit rgb pixel list" <|
      \(list, _) ->
          flatten list
            |> PixelArray.initialize (PixelInfo.initialize RGB 8)
            |> Expect.equal (expected list)

    , fuzz fuzzer "makes pixels from 16 bit rgb pixel list" <|
      \(list, extra) ->
          list8bitTo16bitFlat extra list
            |> PixelArray.initialize (PixelInfo.initialize RGB 16)
            |> Expect.equal (expected list)
    ]


makeRGBAPixelArray =
  let
      fuzzer =
        Fuzz.map2 Tuple.pair rgbaListFuzzer colorRangeFuzzer

      expected list =
        List.map Pixel.fromList list |> Array.fromList
  in
  describe "make RGBA pixels"
    [ fuzz fuzzer "makes pixels from 8 bit rgba pixel list" <|
      \(list, _) ->
          flatten list
            |> PixelArray.initialize (PixelInfo.initialize RGBA 8)
            |> Expect.equal (expected list)

    , fuzz fuzzer "makes pixels from 16 bit rgba pixel list" <|
      \(list, extra) ->
          list8bitTo16bitFlat extra list
            |> PixelArray.initialize (PixelInfo.initialize RGBA 16)
            |> Expect.equal (expected list)
    ]


makeGrayscalePixelArray =
  let
      fuzzer =
        Fuzz.map2 Tuple.pair rgbaFuzzer colorRangeFuzzer

      expected list =
        List.map (\i -> (List.repeat 3 i) ++ [255]) list
          |> List.map Pixel.fromList
          |> Array.fromList
  in
  describe "make grayscale pixels"
    [ fuzz fuzzer "makes pixels from 8 bit grayscale pixel list" <|
      \(list, _) ->
          list
            |> PixelArray.initialize (PixelInfo.initialize Grayscale 8)
            |> Expect.equal (expected list)

    , fuzz fuzzer "makes pixels from 16 bit grayscale pixel list" <|
      \(list, extra) ->
          list8bitTo16bitFlat extra [list]
            |> PixelArray.initialize (PixelInfo.initialize Grayscale 16)
            |> Expect.equal (expected list)
    ]


makeGrayscaleAlphaPixelArray =
  let
      fuzzer =
        Fuzz.map2 Tuple.pair grayAListFuzzer colorRangeFuzzer

      expected list =
        list
          |> List.map (\(i, a) -> List.repeat 3 i ++ [a])
          |> List.map Pixel.fromList
          |> Array.fromList
  in
  describe "make grayscale alpha pixels"
    [ fuzz fuzzer "makes pixels from 8 bit grayscale a pixel list" <|
      \(list, _) ->
          list
            |> List.map (\(i, a) -> [i, a])
            |> flatten
            |> PixelArray.initialize
                (PixelInfo.initialize GrayscaleAlpha 8)
            |> Expect.equal (expected list)

    , fuzz fuzzer "makes pixels from 16 bit grayscale a pixel list" <|
      \(list, extra) ->
          list
            |> List.map (\(i, a) -> [i, a])
            |> list8bitTo16bitFlat extra
            |> PixelArray.initialize
                 (PixelInfo.initialize GrayscaleAlpha 16)
            |> Expect.equal (expected list)
    ]


flatten : List (List a) -> List a
flatten list =
  List.concatMap identity list


list8bitTo16bitFlat : Int -> List (List Int) -> List Int
list8bitTo16bitFlat extra list =
  let
      flat = flatten list
  in
  List.repeat (List.length flat) extra
    |> zip flat
    |> List.concatMap (\(a, b) -> [a, b])
