module Chunk exposing
    (Chunk(..), IhdrData, imageData, ihdrData)


import Bytes exposing (Bytes)
import PixelInfo exposing (PixelInfo)


import Matrix exposing (Dimensions)


type Chunk
  = Ihdr IhdrData
  | Idat Bytes
  | Iend
  | Chunk String Bytes


type alias IhdrData =
  { dimensions: Dimensions
  , pixelInfo: PixelInfo
  , interlaced: Bool
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
