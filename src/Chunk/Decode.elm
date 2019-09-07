module Chunk.Decode exposing (chunksDecoder)


import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing
    (Decoder, Step(..), decode, unsignedInt8, unsignedInt32, andThen)
import Flate exposing (crc32)

import Chunk exposing (..)


chunksDecoder : Decoder (List Chunk)
chunksDecoder =
  Decode.loop [] (\cs -> Decode.map (chunkStep cs) chunkDecoder)


chunkStep : List Chunk -> Chunk -> Step (List Chunk) (List Chunk)
chunkStep cs c =
  case c of
    Iend ->
      Done <| List.reverse <| c :: cs

    _ ->
      Loop <| c :: cs


chunkDecoder : Decoder Chunk
chunkDecoder =
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
  Decode.map identity (dimensions IhdrData)
    |> andThen colorInfo
    |> andThen processing
    |> andThen (Ihdr >> Decode.succeed)


idat : Int -> Decoder Chunk
idat length =
  Decode.map Idat (Decode.bytes length)


dimensions data =
  Decode.map2 data
    (unsignedInt32 BE)
    (unsignedInt32 BE)


colorInfo data =
  Decode.map2 Tuple.pair
    unsignedInt8
    unsignedInt8
      |> andThen color
      |> andThen (data >> Decode.succeed)


processing data =
  Decode.map3 data
    unsignedInt8
    unsignedInt8
    unsignedInt8


color : (Int, Int) -> Decoder Color
color (depth, colorType) =
  let
      decoder allowed fun =
        if List.member depth allowed then
          Decode.succeed <| fun depth
        else
          Decode.fail
  in
  case colorType of
    0 ->
      decoder [ 1, 2, 4, 8, 16 ] Grayscale

    2 ->
      decoder [ 8, 16 ] RGB

    3 ->
      decoder [ 1, 2, 4, 8 ] Indexed

    4 ->
      decoder [ 8, 16 ] GrayscaleA

    6 ->
      decoder [ 8, 16 ] RGBA

    _ ->
      Decode.fail
