module PixelInfo exposing
  ( PixelInfo
  , byteCount
  , bitDepth
  , channels
  , encoder
  , decoder
  )


import Bytes.Encode as Encode exposing (Encoder, sequence, unsignedInt8)
import Bytes.Decode as Decode exposing (Decoder)


type Mode
  = Grayscale
  | RGB
  | Indexed
  | GrayscaleAlpha
  | RGBA


type PixelInfo = PixelInfo Mode Int



bitDepth : PixelInfo -> Int
bitDepth (PixelInfo _ d) = d


channels : PixelInfo -> Int
channels (PixelInfo mode _) =
  case mode of
    Grayscale -> 1
    RGB -> 3
    Indexed -> 1
    GrayscaleAlpha -> 2
    RGBA -> 4


byteCount : PixelInfo -> Int
byteCount info =
  (bitDepth info) * (channels info) // 8


encoder : PixelInfo -> Encoder
encoder color =
  case color of
    PixelInfo Grayscale depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 0 ]

    PixelInfo RGB depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 2 ]

    PixelInfo Indexed depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 3 ]

    PixelInfo GrayscaleAlpha depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 4 ]

    PixelInfo RGBA depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 6 ]


decoder : (Int, Int) -> Decoder PixelInfo
decoder (depth, pixelInfoType) =
  let
      dec allowed fun =
        if List.member depth allowed then
          Decode.succeed <| fun depth
        else
          Decode.fail
  in
  case pixelInfoType of
    0 ->
      dec [ 1, 2, 4, 8, 16 ] <| PixelInfo Grayscale

    2 ->
      dec [ 8, 16 ] <| PixelInfo RGB

    3 ->
      dec [ 1, 2, 4, 8 ] <| PixelInfo Indexed

    4 ->
      dec [ 8, 16 ] <| PixelInfo GrayscaleAlpha

    6 ->
      dec [ 8, 16 ] <| PixelInfo RGBA

    _ ->
      Decode.fail
