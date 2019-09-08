module Chunk exposing
    (Chunk(..), Color(..), ColorMode(..), IhdrData, imageData, ihdrData)


import Bytes exposing (Bytes)
import Flate exposing (crc32)


type Chunk
  = Ihdr IhdrData
  | Idat Bytes
  | Iend
  | Chunk String Bytes


type ColorMode
  = Grayscale
  | RGB
  | Indexed
  | GrayscaleA
  | RGBA


type Color = Color ColorMode Int


type alias IhdrData =
  { width: Int
  , height: Int
  , color: Color
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
