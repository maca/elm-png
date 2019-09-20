module Pixel exposing
    ( Pixel
    , blank
    , rgb
    , rgba
    , grayscale
    , grayscaleAlpha
    , red
    , green
    , blue
    , alpha
    , fromList
    , toList
    )

import List.Extra exposing (getAt)


type Pixel
  = Blank
  | Pixel Int


blank : Pixel
blank = Blank


rgb : Int -> Int -> Int -> Pixel
rgb r g b =
  fromList [r, g, b, 255]


rgba : Int -> Int -> Int -> Int -> Pixel
rgba r g b a =
  fromList [r, g, b, a]


grayscale : Int -> Pixel
grayscale  i =
  fromList [i, i, i, 255]


grayscaleAlpha : Int -> Int -> Pixel
grayscaleAlpha  i a =
  fromList [i, i, i, a]


toList : Pixel -> List Int
toList pixel =
  case pixel of
    Pixel int ->
      let
        r = int // b24
        redOffset = int - r * b24
        g = (redOffset - remainderBy b16 redOffset) // b16
        greenOffset = redOffset - g * b16
        b = (greenOffset - remainderBy b8 greenOffset) // b8
      in
      [ r, g, b, greenOffset - b * b8 ]

    Blank ->
      []


b24 : Int
b24 = 2^24


b16 : Int
b16 = 2^16


b8 : Int
b8 = 2^8


red : Pixel -> Int
red pixel =
  toList pixel |> getAt 0 |> Maybe.withDefault 0


green : Pixel -> Int
green pixel =
  toList pixel |> getAt 1 |> Maybe.withDefault 0


blue : Pixel -> Int
blue pixel =
  toList pixel |> getAt 2 |> Maybe.withDefault 0


alpha : Pixel -> Int
alpha pixel =
  toList pixel |> getAt 3 |> Maybe.withDefault 0


fromList : List Int -> Pixel
fromList ints =
  if List.length ints /= 4 || List.any (\i -> i > 255) ints
                           || List.any (\i -> i < 0) ints then
    Blank
  else
    Pixel <| List.foldl fromListStep 0 ints


fromListStep : Int -> Int -> Int
fromListStep curr res =
  res * 256 + curr
