module Chunk exposing (Chunk(..), Dimensions, ColorInfo, Processing)


import Bytes exposing (Bytes)
import Flate exposing (crc32)


type Chunk
  = Ihdr Dimensions ColorInfo Processing
  | Idat Bytes
  | Iend
  | Chunk String Bytes


type alias Dimensions =
  { width: Int
  , height: Int
  }


type alias ColorInfo =
  { bitDepth: Int
  , colorType: Int
  }


type alias Processing =
  { compression: Int
  , filter: Int
  , interlace: Int
  }
