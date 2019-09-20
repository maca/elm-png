module Png exposing
  ( Png
  , fromBytes
  , toBytes
  , toImage
  , fromImage
  , toUrl
  )


import Array exposing (Array)
import Bytes.Decode as Decode exposing (Decoder, Step(..), unsignedInt8)
import Bytes.Encode as Encode exposing (Encoder, encode, sequence)
import Bytes exposing (Bytes, Endianness(..))
import Base64


import Adam7 exposing (passDimensions, mergePasses)
import Chunk exposing (Chunk(..), IhdrData)
import Chunk.Decode exposing (chunksDecoder)
import Chunk.Encode exposing (chunksEncoder)
import Filter exposing (Filter)
import Image exposing (Image)
import Matrix exposing (Dimensions)
import Pixel exposing (Pixel)
import PixelArray
import PixelInfo exposing (PixelInfo, Mode(..))
import Decode.Loop exposing (list, iterate)
import Png.Signature as Signature


import Flate exposing
    (Encoding(..), inflateZlib, deflateZlib, deflateZlibWithOptions)


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
    Just { dimensions, pixelInfo, interlaced } ->
      let
          decoder =
            if interlaced then
              deinterlace pixelInfo dimensions

            else
              imageDecoder pixelInfo dimensions
      in
      imageData png |> Maybe.andThen (Decode.decode decoder)

    _ ->
      Nothing


ihdr : Png -> Maybe IhdrData
ihdr (Png chunks) =
  List.head chunks |> Maybe.andThen Chunk.ihdrData


deinterlace : PixelInfo -> Dimensions -> Decoder Image
deinterlace pixelInfo dimensions =
  let
      passDecoder n =
        imageDecoder pixelInfo (Adam7.passDimensions n dimensions)
  in
  iterate (List.range 0 6) passDecoder
    |> Decode.andThen (mergePasses dimensions >> Decode.succeed)


imageDecoder : PixelInfo -> Dimensions -> Decoder Image
imageDecoder pixelInfo ({ height } as dimensions) =
  let
      pixels ln acc =
        PixelArray.initialize pixelInfo ln |> Array.append acc

      process =
        Filter.revert pixelInfo
          >> List.foldr pixels Array.empty
          >> Image.fromArray dimensions
          >> Decode.succeed
  in
  list height (lineDecoder dimensions pixelInfo)
    |> Decode.andThen process


lineDecoder : Dimensions -> PixelInfo -> Decoder (Filter, List Int)
lineDecoder { width } pixelInfo =
  Decode.map2 Tuple.pair
    (Filter.decoder pixelInfo)
    (list (width * PixelInfo.byteCount pixelInfo) unsignedInt8)


imageData : Png -> Maybe Bytes
imageData (Png chunks) =
  List.filterMap (Chunk.imageData >> Maybe.map Encode.bytes) chunks
    |> sequence
    |> encode
    |> inflateZlib


pngDecoder : Decoder Png
pngDecoder =
  Signature.decoder
    |> Decode.andThen (always chunksDecoder)
    |> Decode.andThen (Png >> Decode.succeed)


--- from image

fromImage : Image -> Png
fromImage image =
  let
    info =
      { dimensions = Image.dimensions image
      , pixelInfo = PixelInfo.initialize RGBA 8
      , interlaced = False
      }

    idat =
      Image.foldLines lineFoldStep image
        |> sequence
        |> encode
        |> deflateZlib
  in
  [ Ihdr info, Idat idat, Iend ] |> Png


lineFoldStep : Array Pixel -> Encoder
lineFoldStep line =
  (Encode.unsignedInt8 0) ::
    (Array.foldr (pixelEncoder >> (::)) [] line)
      |> sequence


pixelEncoder : Pixel -> Encoder
pixelEncoder pixel =
  Pixel.toList pixel
    |> List.map Encode.unsignedInt8
    |> sequence



toUrl : Png -> String
toUrl png =
  let
      base64 =
        toBytes png
          |> Base64.fromBytes
          |> Maybe.withDefault ""
  in
  "data:*/*;base64," ++ base64
