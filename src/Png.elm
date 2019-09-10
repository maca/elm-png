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
import Filter exposing (Filter)
import PixelInfo exposing (PixelInfo, channels, bitDepth)


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
  list (List.length signature) unsignedInt8


pngDecoder : Decoder Png
pngDecoder =
  signatureDecoder
    |> Decode.andThen
        (\s -> if s == signature then Decode.succeed s else Decode.fail)
    |> Decode.andThen (always chunksDecoder)
    |> Decode.andThen (Decode.succeed << Png)


list : Int -> Decoder a -> Decoder (List a)
list length decoder =
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


pixels : Png -> (List (List (List Int)))
pixels png =
  case ihdr png of
    Just ihdrData ->
      imageData png
        |> Maybe.andThen (decode (linesDecoder ihdrData))
        |> Maybe.withDefault []

    Nothing ->
      []


linesDecoder : IhdrData -> Decoder (List (List (List Int)))
linesDecoder ({ height, pixelInfo } as ihdrData) =
  list height (line ihdrData)
    |> Decode.andThen (unfilter pixelInfo >> Decode.succeed)


line : IhdrData -> Decoder (Filter, List Int)
line { width, pixelInfo } =
  Decode.map2 Tuple.pair
    (Filter.decoder pixelInfo)
    (list (width * PixelInfo.byteCount pixelInfo) unsignedInt8)


unfilter : PixelInfo -> List (Filter, List Int)
                     -> List (List (List Int))
unfilter pixelInfo lines =
  List.foldl unfilterLineStep ([], []) lines
    |> Tuple.second
    |> List.foldl (linePixels pixelInfo) []


unfilterLineStep : (Filter, List Int) -> (List Int, (List (List Int)))
                                      -> (List Int, (List (List Int)))
unfilterLineStep (filter, ln) (prevLn, lineList) =
  List.foldl (unfilterByte filter prevLn) [] ln
    |> List.reverse
    |> (\l -> l :: lineList)
    |> Tuple.pair ln


unfilterByte : Filter -> List Int -> Int -> List Int -> List Int
unfilterByte filter prevLn byte byteList =
  Filter.revert filter prevLn byteList byte :: byteList


linePixels : PixelInfo -> List Int
                       -> List (List (List Int))
                       -> List (List (List Int))
linePixels pixelInfo ln lineList =
  groupsOf 3 ln :: lineList
