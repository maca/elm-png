module Chunk exposing (Chunk(..), Color(..), IhdrData, imageData)


import Bytes exposing (Bytes)
import Flate exposing (crc32)


type Chunk
  = Ihdr IhdrData
  | Idat Bytes
  | Iend
  | Chunk String Bytes


type Color
  = Grayscale Int
  | RGB Int
  | Indexed Int
  | GrayscaleA Int
  | RGBA Int


type alias IhdrData =
  { width: Int
  , height: Int
  , color: Color
  , compression: Int
  , filter: Int
  , interlace: Int
  }


imageData : Chunk -> Maybe Bytes
imageData chunk =
  case chunk of
    Idat data -> Just data
    _ -> Nothing
