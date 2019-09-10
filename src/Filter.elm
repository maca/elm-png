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


revert filter prevLn prevBytes byte =
  case filter of
    None ->
      byte

    Sub offset ->
      getA offset prevBytes |> sum byte

    Up offset ->
      Debug.todo "crash"

    Average offset ->
      Debug.todo "crash"

    Paeth offset ->
      Debug.todo "crash"


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


getA offset prevBytes =
  getAt (offset - 1) prevBytes |> Maybe.withDefault 0


sum first second =
  first + second |> remainderBy 256
