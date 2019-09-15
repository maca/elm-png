-- module Png exposing (Png, fromBytes, toBytes)
module Png exposing (..)


import Array exposing (Array)
import Bytes.Decode as Decode exposing
    (Decoder, Step(..), unsignedInt8, unsignedInt16)
import Bytes.Encode as Encode exposing (Encoder, encode, sequence)
import Bytes exposing (Bytes, Endianness(..))
import List.Extra exposing (groupsOf, getAt)


import Adam7 exposing (passDimensions, mergePasses)
import Chunk exposing (..)
import Chunk.Decode exposing (chunksDecoder)
import Chunk.Encode exposing (chunksEncoder)
import Filter exposing (Filter)
import Image exposing (Image)
import Matrix exposing (Dimensions)
import Pixel exposing (Pixel)
import PixelInfo exposing (PixelInfo, channels, bitDepth)
import Decode.Loop exposing (..)
import Png.Signature as Signature exposing (..)


import Flate exposing (inflateZlib, deflateZlib)


type Png = Png (List Chunk)


fromBytes : Bytes -> Maybe Png
fromBytes =
  Decode.decode pngDecoder


toBytes : Png -> Bytes
toBytes (Png chunks) =
  sequence [ Signature.encoder, chunksEncoder chunks ] |> encode


toImage : Png -> Maybe Image
toImage png =
  case ihdr png of
    Just ({ dimensions, pixelInfo, interlaced } as ihdrData) ->
      let
          decoder =
            if interlaced then
              deinterlace pixelInfo dimensions

            else
              image pixelInfo dimensions
      in
      imageData png |> Maybe.andThen (Decode.decode decoder)

    Nothing ->
      Nothing


deinterlace pixelInfo dimensions =
  Decode.loop ( List.range 0 6, [] )
    (iterStep <| passDecoder pixelInfo dimensions)
       |> Decode.andThen (mergePasses dimensions >> Decode.succeed)


passDecoder : PixelInfo -> Dimensions -> Int -> Decoder Image
passDecoder pixelInfo dimensions n =
  image pixelInfo (Adam7.passDimensions n dimensions)


image : PixelInfo -> Dimensions -> Decoder Image
image pixelInfo ({ height } as dimensions) =
  list height (line dimensions pixelInfo)
    |> Decode.andThen
       (unfilter pixelInfo >> Image.fromArray dimensions
                           >> Decode.succeed)


line : Dimensions -> PixelInfo -> Decoder (Filter, List Int)
line { width } pixelInfo =
  Decode.map2 Tuple.pair
    (Filter.decoder pixelInfo)
    (list (width * PixelInfo.byteCount pixelInfo) unsignedInt8)


unfilter : PixelInfo -> List (Filter, List Int) -> Array Pixel
unfilter pixelInfo lines =
  List.foldl unfilterLine [] lines
    |> List.foldr (appendPixels pixelInfo) Array.empty


unfilterLine : (Filter, List Int) -> List (List Int) -> List (List Int)
unfilterLine (filter, ln) acc =
  let
      prevLn =
        List.head acc |> Maybe.withDefault []

      unfiltered =
        List.foldl (unfilterByte filter prevLn) [] ln |> List.reverse
  in
  unfiltered :: acc


unfilterByte : Filter -> List Int -> Int -> List Int -> List Int
unfilterByte filter prevLn byte acc =
  Filter.revert filter (List.length acc) prevLn acc byte :: acc


appendPixels : PixelInfo -> List Int -> Array Pixel -> Array Pixel
appendPixels pixelInfo ln pxs =
  groupsOf 3 ln
    |> Array.fromList
    |> Array.append pxs


imageData : Png -> Maybe Bytes
imageData (Png chunks) =
  List.filterMap (Chunk.imageData >> Maybe.map Encode.bytes) chunks
    |> sequence
    |> encode
    |> inflateZlib


pngDecoder : Decoder Png
pngDecoder =
  Signature.decoder
    |> Decode.andThen
        (\s -> if s == signature then Decode.succeed s else Decode.fail)
    |> Decode.andThen (always chunksDecoder)
    |> Decode.andThen (Png >> Decode.succeed)


ihdr : Png -> Maybe IhdrData
ihdr (Png chunks) =
  chunks |> List.head |> Maybe.andThen Chunk.ihdrData
