module Chunk.Encode exposing (chunksEncoder)


import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing
    (Encoder, encode, sequence, string, unsignedInt8, unsignedInt32)

import Chunk exposing (..)

import Flate exposing (crc32)


chunksEncoder : List Chunk -> Encoder
chunksEncoder cs =
  sequence <| List.map chunkEncoder cs


chunkEncoder : Chunk -> Encoder
chunkEncoder c =
  case c of
    Ihdr dimensions colorInfo processing ->
      chunkEncoderHelp "IHDR"
        <| encode
        <| ihdrDataEncoder dimensions colorInfo processing

    Idat data ->
      chunkEncoderHelp "IDAT" data

    Iend ->
      chunkEncoderHelp "IEND" <| encode <| string ""

    Chunk chunkType data ->
      chunkEncoderHelp chunkType data


chunkEncoderHelp : String -> Bytes -> Encoder
chunkEncoderHelp chunkType data =
  let
      bytes =
        encode <| sequence [ Encode.string chunkType, Encode.bytes data ]
  in
      sequence
        [ unsignedInt32 BE <| Bytes.width data
        , Encode.bytes bytes
        , unsignedInt32 BE <| crc32 bytes
        ]


ihdrDataEncoder : Dimensions -> ColorInfo -> Processing -> Encoder
ihdrDataEncoder dimensions colorInfo processing =
  sequence
    [ dimensionsEncoder dimensions
    , colorInfoEncoder colorInfo
    , processingEncoder processing
    ]


dimensionsEncoder : Dimensions -> Encoder
dimensionsEncoder { width, height } =
  sequence
    [ unsignedInt32 BE width
    , unsignedInt32 BE height
    ]


colorInfoEncoder : ColorInfo -> Encoder
colorInfoEncoder { bitDepth, colorType } =
  sequence [ unsignedInt8 bitDepth, unsignedInt8 colorType ]


processingEncoder : Processing -> Encoder
processingEncoder { compression, filter, interlace } =
  sequence
    [ unsignedInt8 compression
    , unsignedInt8 filter
    , unsignedInt8 interlace
    ]
