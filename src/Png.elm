module Png exposing (Png, fromBytes)


import Bytes.Decode as Decode exposing (Decoder, Step(..), unsignedInt8)
import Bytes exposing (Bytes)

import Chunk exposing (Chunk)
import Chunk.Decode exposing (chunks)
import Chunk.Encode exposing (..)


type Png = Png (List Chunk)


signature : List Int
signature =
  [ 137, 80, 78, 71, 13, 10, 26, 10 ]


fromBytes : Bytes -> Maybe Png
fromBytes =
  Decode.decode pngDecoder


pngDecoder : Decoder Png
pngDecoder =
  listDecoder (List.length signature) unsignedInt8
    |> Decode.andThen
        (\s -> if s == signature then Decode.succeed s else Decode.fail)
    |> Decode.andThen (always chunks)
    |> Decode.andThen (Decode.succeed << Png)


listDecoder : Int -> Decoder a -> Decoder (List a)
listDecoder length decoder =
  Decode.loop (length, []) (step decoder)


step : Decoder a -> (Int, List a)
                 -> Decoder (Step (Int, List a) (List a))
step decoder (n, xs) =
  if n <= 0 then
    Decode.succeed (Done <| List.reverse xs)
  else
    Decode.map (\x -> Loop (n - 1, x :: xs)) decoder
