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
    Ihdr data ->
      chunkEncoderHelp "IHDR"
        <| encode
        <| ihdrDataEncoder data

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


ihdrDataEncoder : IhdrData -> Encoder
ihdrDataEncoder data =
  sequence
    [ dimensionsEncoder data
    , colorInfoEncoder data
    , processingEncoder data
    ]


dimensionsEncoder : IhdrData -> Encoder
dimensionsEncoder { width, height } =
  sequence
    [ unsignedInt32 BE width
    , unsignedInt32 BE height
    ]


colorInfoEncoder : IhdrData -> Encoder
colorInfoEncoder { color } =
  case color of
    Grayscale depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 0 ]

    RGB depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 2 ]

    Indexed depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 3 ]

    GrayscaleA depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 4 ]

    RGBA depth ->
      sequence [ unsignedInt8 depth, unsignedInt8 6 ]


processingEncoder : IhdrData -> Encoder
processingEncoder { compression, filter, interlace } =
  sequence
    [ unsignedInt8 compression
    , unsignedInt8 filter
    , unsignedInt8 interlace
    ]
