module Chunk.Decode exposing (chunksDecoder)


import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing
    (Decoder, Step(..), decode, unsignedInt8, unsignedInt32, andThen)


import Chunk exposing (Chunk(..), IhdrData)
import Matrix exposing (Dimensions)
import PixelInfo


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
anciliary length chunkType _ =
  Decode.bytes length
    |> andThen
        (\data -> Decode.succeed <| Chunk chunkType data)


ihdr : Decoder Chunk
ihdr =
  dimensions IhdrData
    |> andThen pixelInfo
    |> andThen compression
    |> andThen filter
    |> andThen interlaced
    |> andThen (Ihdr >> Decode.succeed)


idat : Int -> Decoder Chunk
idat length =
  Decode.map Idat (Decode.bytes length)


dimensions data =
  Decode.map2 Dimensions
    (unsignedInt32 BE)
    (unsignedInt32 BE)
      |> andThen (data >> Decode.succeed)


pixelInfo data =
  Decode.map2 Tuple.pair
    unsignedInt8
    unsignedInt8
      |> andThen PixelInfo.decoder
      |> andThen (data >> Decode.succeed)


compression data =
  unsignedInt8 -- discard since only one value is specified
    |> andThen (always <| Decode.succeed data)


filter = compression -- also discard


interlaced data =
  unsignedInt8
    |> andThen bool
    |> andThen (data >> Decode.succeed)


bool : Int -> Decoder Bool
bool i =
  case i of
    0 -> Decode.succeed False
    1 -> Decode.succeed True
    _ -> Decode.fail
