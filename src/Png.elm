module Png exposing (..)

import Bytes.Decode as Decode exposing (Decoder)

import Chunk exposing (Chunk)
import Chunk.Decode exposing (..)


type Png = Png (List Chunk)



decoder : Decoder Png
decoder =
  signatureDecoder
    |> Decode.andThen chunks
    |> Decode.andThen (Decode.succeed << Png)


fromBytes bytes =
  Decode.decode decoder bytes
