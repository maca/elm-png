-- module Png exposing (Png, fromBytes, toBytes)
module Png exposing (..)


import Bytes.Decode as Decode exposing
    (Decoder, Step(..), decode, unsignedInt8, unsignedInt16)
import Bytes.Encode as Encode exposing (Encoder, encode, sequence)
import Bytes exposing (Bytes, Endianness(..))
import List.Extra exposing (groupsOf, getAt)



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
        |> Maybe.andThen (decode (scanlinesDecoder ihdrData))
        |> Maybe.withDefault []

    Nothing ->
      []


scanlinesDecoder ({ width, height, color } as ihdrData) =
  let
      scanlineWidth =
        (pixelWidth color) * width + 1
  in
  Decode.loop
    (height, [])
    (step <| Decode.loop (scanlineWidth, []) (step unsignedInt8))
      |> Decode.andThen (scanlinePixels ihdrData)


scanlinePixels ihdrData scanlines =
  List.foldl (revertFilter ihdrData) ([], Nothing) scanlines
   |> Tuple.first
   |> List.reverse
   |> List.map (groupsOf 3)
   |> Decode.succeed


revertFilter ihdrData scanline (acc, unfiltered) =
  case scanline of
    filterType :: pixelBytes ->
      let
          fun byte bytes =
            (filter filterType ihdrData unfiltered bytes byte) :: bytes

          reconstructed =
            List.foldl fun [] pixelBytes |> List.reverse
      in
      ( reconstructed :: acc, Just pixelBytes )

    _ ->
      ( [], Nothing )


filter filterType { color} =
  let
      filterOffset =
        if (depth color) < 8 then 1 else channels color
  in
  case filterType of
    0 -> none filterOffset
    1 -> sub filterOffset
    _ -> none filterOffset


none _ _ _ byte =
  byte


sub offset _ prevBytes byte =
  (getA offset prevBytes) + byte
    |> remainderBy 256


getA offset prevBytes =
  getAt (offset - 1) prevBytes
    |> Maybe.withDefault 0


pixelWidth : Color -> Int
pixelWidth color =
  (depth color) * (channels color) // 8


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
