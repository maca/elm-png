module PixelArray exposing (initialize)


import Array exposing (Array)


import Pixel exposing (Pixel, fromList)
import PixelInfo exposing (PixelInfo)


initialize : PixelInfo -> List Int -> Array Pixel
initialize pixelInfo ln =
  let
      channels =
        PixelInfo.channels pixelInfo

      bitDepth =
        PixelInfo.bitDepth pixelInfo

      list =
        case bitDepth of
          8 ->
            ln

          16 ->
            keepSignificant [] ln |> List.reverse

          _ ->
            []
  in
  makePixels channels (pixelArrayHelp bitDepth) Array.empty list


keepSignificant : List Int -> List Int -> List Int
keepSignificant acc xs =
  case List.take 2 xs of
    [ byte, _ ] ->
      keepSignificant (byte :: acc) <| List.drop 2 xs

    _ ->
      acc


pixelArrayHelp : Int -> List Int -> Pixel
pixelArrayHelp _ ints =
  case ints of
    [r, g, b] ->
      Pixel.fromList [r, g, b, 255]

    [r, g, b, a] ->
      Pixel.fromList [r, g, b, a]

    [i] ->
      Pixel.fromList [i, i, i, 255]

    [i, a] ->
      Pixel.fromList [i, i, i, a]

    _ ->
      Pixel.blank


makePixels : Int -> (List Int -> Pixel)
                 -> Array Pixel
                 -> List Int
                 -> Array Pixel
makePixels size fun acc xs =
  let
      list = List.take size xs
  in
  if size > 0 && size == List.length list then
    makePixels size fun (Array.push (fun list) acc) (List.drop size xs)

  else
    acc
