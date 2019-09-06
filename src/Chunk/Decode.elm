module Chunk.Decode exposing (..)


import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing
    (Decoder, Step(..), decode, unsignedInt8, unsignedInt32)
import Bytes.Encode as Encode exposing (Encoder)
import Chunk exposing (..)



signature : List Int
signature =
  [ 137, 80, 78, 71, 13, 10, 26, 10 ]


chunks : List Int -> Decoder (List Chunk)
chunks header =
  if header == signature then
    Decode.loop [] chunkStep
  else
    Decode.fail


chunkStep : List Chunk -> Decoder (Step (List Chunk) (List Chunk))
chunkStep cs =
  Decode.map (chunkStepHelp cs) chunk


chunkStepHelp : List Chunk -> Chunk -> (Step (List Chunk) (List Chunk))
chunkStepHelp cs c =
  case c of
    Iend ->
      Done <| List.reverse <| c :: cs

    _ ->
      Loop <| c :: cs


signatureDecoder : Decoder (List Int)
signatureDecoder =
  listDecoder (List.length signature) unsignedInt8


chunk : Decoder Chunk
chunk =
  Decode.map2 Tuple.pair
    (unsignedInt32 BE)
    (Decode.string 4)
      |> Decode.andThen chunkHelp


chunkHelp : (Int, String) -> Decoder Chunk
chunkHelp (length, chunkType) =
  Decode.map2 (Chunk length chunkType)
    (Decode.bytes length)
    (unsignedInt32 BE)
      |> Decode.andThen chunkMap


chunkMap : Chunk -> Decoder Chunk
chunkMap c =
  case c of
    Chunk _ "IHDR" bytes _ ->
      Decode.map3 Ihdr dimensions colorInfo processing
        |> (\decoder -> decode decoder bytes)
        |> Maybe.map Decode.succeed
        |> Maybe.withDefault Decode.fail

    Chunk _ "IDAT" bytes _ ->
        Decode.succeed c

    Chunk _ "IEND" _ _ ->
        Decode.succeed Iend

    Chunk length _ _ _ ->
        Decode.succeed c

    _ ->
        Decode.fail


dimensions : Decoder Dimensions
dimensions =
  Decode.map2 Dimensions
    (unsignedInt32 BE)
    (unsignedInt32 BE)


colorInfo : Decoder ColorInfo
colorInfo =
  Decode.map2 ColorInfo
    unsignedInt8
    unsignedInt8


processing : Decoder Processing
processing =
  Decode.map3 Processing
    unsignedInt8
    unsignedInt8
    unsignedInt8


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
