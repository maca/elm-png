module Png exposing (Png, fromBytes, toBytes)


import Bytes.Decode as Decode exposing
    (Decoder, Step(..), decode, unsignedInt8)
import Bytes.Encode as Encode exposing (Encoder, encode, sequence)

import Bytes exposing (Bytes)

import Chunk exposing (Chunk)
import Chunk.Decode
import Chunk.Encode exposing (chunksEncoder)


type Png = Png (List Chunk)


fromBytes : Bytes -> Maybe Png
fromBytes =
  decode pngDecoder


toBytes : Png -> Bytes
toBytes (Png chunks) =
  encode <| sequence [ signatureEncoder, chunksEncoder chunks ]


signature : List Int
signature =
  [ 137, 80, 78, 71, 13, 10, 26, 10 ]


signatureEncoder : Encoder
signatureEncoder =
  Encode.sequence <| List.map Encode.unsignedInt8 signature


signatureDecoder : Decoder (List Int)
signatureDecoder =
  listDecoder (List.length signature) unsignedInt8


pngDecoder : Decoder Png
pngDecoder =
  signatureDecoder
    |> Decode.andThen
        (\s -> if s == signature then Decode.succeed s else Decode.fail)
    |> Decode.andThen (always Chunk.Decode.chunks)
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
