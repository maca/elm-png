module Chunk exposing
    (Chunk(..), IhdrData, imageData, ihdrData)


import Bytes exposing (Bytes)
import Flate exposing (crc32)
import PixelInfo exposing (PixelInfo)


type Chunk
  = Ihdr IhdrData
  | Idat Bytes
  | Iend
  | Chunk String Bytes


type alias IhdrData =
  { width: Int
  , height: Int
  , pixelInfo: PixelInfo
  , interlacing: Bool
  }


imageData : Chunk -> Maybe Bytes
imageData chunk =
  case chunk of
    Idat data -> Just data
    _ -> Nothing


ihdrData : Chunk -> Maybe IhdrData
ihdrData chunk =
  case chunk of
    Ihdr data -> Just data
    _ -> Nothing
