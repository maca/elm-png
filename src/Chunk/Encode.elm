module Chunk.Encode exposing (chunksEncoder)


import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing
    (Encoder, encode, sequence, string, unsignedInt8, unsignedInt32)
import Flate exposing (crc32)


import Chunk exposing (Chunk(..), IhdrData)
import Matrix exposing (Dimensions)
import PixelInfo


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
ihdrDataEncoder ({ dimensions, pixelInfo } as data) =
  sequence
    [ dimensionsEncoder dimensions
    , PixelInfo.encoder pixelInfo
    , processingEncoder data
    ]


dimensionsEncoder : Dimensions -> Encoder
dimensionsEncoder { width, height } =
  sequence
    [ unsignedInt32 BE width
    , unsignedInt32 BE height
    ]


processingEncoder : IhdrData -> Encoder
processingEncoder { interlaced } =
  sequence
    [ unsignedInt8 0
    , unsignedInt8 0
    , boolIntEncoder interlaced
    ]


boolIntEncoder : Bool -> Encoder
boolIntEncoder bool =
  unsignedInt8 <| if bool then 1 else 0
