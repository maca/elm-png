module Filter exposing (..)


import Bytes.Decode as Decode exposing (Decoder, unsignedInt8)
import List.Extra exposing (getAt)

import PixelInfo exposing (PixelInfo, bitDepth, channels)


type Filter
  = None
  | Sub Int
  | Up Int
  | Average Int
  | Paeth Int


revert filter index prevLn prevBytes byte =
  case filter of
    None ->
      byte

    Sub offset ->
      get (offset - 1) prevBytes |> sum byte

    Up _ ->
      get index prevLn |> sum byte

    Average offset ->
      let
        a = get (offset - 1) prevBytes
        b = get index prevLn
      in
        sum ((a + b) // 2) byte

    Paeth offset ->
      let
        a = get (offset - 1) prevBytes
        b = get index prevLn
        c = get (index - offset) prevLn
        p = a + b - c
        pa = p - a |> abs
        pb = p - b |> abs
        pc = p - c |> abs
      in
      if pa <= pb && pa <= pc then
        sum a byte

      else if pb <= pc then
        sum b byte

      else
        sum c byte


get index bytes =
  getAt index bytes |> Maybe.withDefault 0


sum first second =
  first + second |> remainderBy 256


decoder : PixelInfo -> Decoder Filter
decoder pixelInfo =
  let
      offset =
        if (bitDepth pixelInfo) < 8 then 1 else channels pixelInfo

      dec filterType =
        case filterType of
          0 -> Decode.succeed None
          1 -> Decode.succeed <| Sub offset
          2 -> Decode.succeed <| Up offset
          3 -> Decode.succeed <| Average offset
          4 -> Decode.succeed <| Paeth offset
          _ -> Decode.fail
  in
      unsignedInt8 |> Decode.andThen dec
