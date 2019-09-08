-- module Png exposing (Png, fromBytes, toBytes)
module Png exposing (..)


import Bytes.Decode as Decode exposing
    (Decoder, Step(..), decode, unsignedInt8, unsignedInt16)
import Bytes.Encode as Encode exposing (Encoder, encode, sequence)
import Bytes exposing (Bytes, Endianness(..))



-- import Chunk exposing (Chunk, IhdrData)
import Chunk exposing (..)
import Chunk.Decode exposing (chunksDecoder)
import Chunk.Encode exposing (chunksEncoder)
import Image exposing (..)


import Flate exposing (inflateZlib, deflateZlib)


type Png = Png (List Chunk)


fromBytes : Bytes -> Maybe Png
fromBytes =
  decode pngDecoder


toBytes : Png -> Bytes
toBytes (Png chunks) =
  encode <| sequence [ signatureEncoder, chunksEncoder chunks ]


imageData : Png -> Maybe Bytes
imageData (Png chunks) =
  List.filterMap (Chunk.imageData >> Maybe.map Encode.bytes) chunks
    |> sequence
    |> encode
    |> inflateZlib


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
    |> Decode.andThen (always chunksDecoder)
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






ihdr : Png -> Maybe IhdrData
ihdr (Png chunks) =
  chunks |> List.head |> Maybe.andThen Chunk.ihdrData


pixels png =
  case ihdr png of
    Just ihdrData ->
      imageData png
        |> Maybe.andThen (decode (imageDataDecoder ihdrData))
        |> Maybe.withDefault []

    Nothing ->
      []


-- imageDataDecoder : IhdrData -> Decoder
imageDataDecoder ({ height, color, width } as ihdrData) =
  let
      decoder =
        unsignedInt8 -- filterByte
          |> Decode.andThen (scanlineDecoder ihdrData)
  in
  scanlinesDecoder height decoder



depth : Color -> Int
depth (Color _ d) = d


channels : Color -> Int
channels (Color mode _) =
  case mode of
    Grayscale -> 1
    RGB -> 3
    Indexed -> 1
    GrayscaleA -> 2
    RGBA -> 4


-- scanlineDecoder :  -> Decoder (List a)
scanlineDecoder { color, width } filterByte =
  let
    channelDecoder =
      case depth color of
        16 -> unsignedInt16 BE
        _  -> unsignedInt8

    pixelDecoder =
      listDecoder (channels color) channelDecoder
  in
    Decode.loop (32, []) (step pixelDecoder)


scanlinesDecoder : Int -> Decoder a -> Decoder (List a)
scanlinesDecoder length decoder =
  Decode.loop (length, []) (scanlineStep decoder)


scanlineStep : Decoder a
             -> (Int, List a)
             -> Decoder (Step (Int, List a) (List a))
scanlineStep decoder (n, scanlines) =
  if n <= 0 then
    Decode.succeed (Done <| List.reverse scanlines)
  else
    Decode.map (\s -> Loop (n - 1, s :: scanlines)) decoder
