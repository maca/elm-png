module Chunk exposing (..)
import Bytes exposing (Bytes)


type Chunk
  = Ihdr Dimensions ColorInfo Processing
  | Idat Int String Bytes Int
  | Iend
  | Chunk Int String Bytes Int


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
