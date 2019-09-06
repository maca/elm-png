module Chunk.Decode exposing (chunk, chunks)


import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing
    (Decoder, Step(..), decode, unsignedInt8, unsignedInt32, andThen)
import Chunk exposing (..)

import Flate exposing (crc32)



chunks : Decoder (List Chunk)
chunks =
  Decode.loop [] (\cs -> Decode.map (chunkStep cs) chunk)


chunkStep : List Chunk -> Chunk -> Step (List Chunk) (List Chunk)
chunkStep cs c =
  case c of
    Iend ->
      Done <| List.reverse <| c :: cs

    _ ->
      Loop <| c :: cs


chunk : Decoder Chunk
chunk =
  unsignedInt32 BE -- data length
    |> andThen rawChunk
    |> andThen verify
    |> andThen typedChunk


rawChunk : Int -> Decoder (Int, Bytes, Int)
rawChunk length =
  Decode.map2 (\bytes crc -> (length, bytes, crc))
    (Decode.bytes <| length + 4)
    (unsignedInt32 BE) -- crc


verify : (Int, Bytes, Int) -> Decoder (Int, Bytes, Int)
verify ((_, bytes, crc) as triple) =
  if crc == crc32 bytes then
    Decode.succeed triple
  else
    Decode.fail


typedChunk : (Int, Bytes, Int) -> Decoder Chunk
typedChunk (length, bytes, crc) =
  let
      decodeChunk decoder =
        decode (Decode.string 4 |> andThen (always decoder)) bytes
          |> Maybe.map Decode.succeed
          |> Maybe.withDefault Decode.fail

  in
  case decode (Decode.string 4) bytes of
    Just "IHDR" ->
      decodeChunk ihdr

    Just "IDAT" ->
      decodeChunk <| idat length

    Just "IEND" ->
      Decode.succeed Iend

    Just chunkType ->
      decodeChunk <| anciliary length chunkType crc

    Nothing ->
      Decode.fail


anciliary : Int -> String -> Int -> Decoder Chunk
anciliary length chunkType crc =
  Decode.bytes length
    |> andThen
        (\data -> Decode.succeed <| Chunk chunkType data)


ihdr : Decoder Chunk
ihdr =
  Decode.map3 Ihdr dimensions colorInfo processing


idat : Int -> Decoder Chunk
idat length =
  Decode.map Idat (Decode.bytes length)


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
