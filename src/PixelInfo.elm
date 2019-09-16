module PixelInfo exposing
  ( PixelInfo(..)
  , Mode(..)
  , initialize
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


type PixelInfo
  = PixelInfo Mode Int


initialize : Mode -> Int -> PixelInfo
initialize mode depth =
  let
      build allowed =
        if List.member depth allowed then
          PixelInfo mode depth
        else
          PixelInfo RGBA 8
  in
  case mode of
    Grayscale ->
      build [ 1, 2, 4, 8, 16 ]

    RGB ->
      build [ 8, 16 ]

    Indexed ->
      build [ 1, 2, 4, 8 ]

    GrayscaleAlpha ->
      build [ 8, 16 ]

    RGBA ->
      build [ 8, 16 ]


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
      dec mode =
        initialize mode depth
          |> Decode.succeed
  in
  case pixelInfoType of
    0 -> Grayscale |> dec
    2 -> RGB |> dec
    3 -> Indexed |> dec
    4 -> GrayscaleAlpha |> dec
    6 -> RGBA |> dec
    _ -> Decode.fail
