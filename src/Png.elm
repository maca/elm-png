module Png exposing (Png, fromBytes, toBytes, toImage)


import Array
import Bytes.Decode as Decode exposing
    (Decoder, Step(..), unsignedInt8)
import Bytes.Encode as Encode exposing (encode, sequence)
import Bytes exposing (Bytes, Endianness(..))


import Adam7 exposing (passDimensions, mergePasses)
import Chunk exposing (Chunk, IhdrData)
import Chunk.Decode exposing (chunksDecoder)
import Chunk.Encode exposing (chunksEncoder)
import Filter exposing (Filter)
import Image exposing (Image)
import Matrix exposing (Dimensions)
import PixelArray
import PixelInfo exposing (PixelInfo)
import Decode.Loop exposing (list, iterate)
import Png.Signature as Signature


import Flate exposing (inflateZlib)


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
              image pixelInfo dimensions
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
        image pixelInfo (Adam7.passDimensions n dimensions)
  in
  iterate (List.range 0 6) passDecoder
    |> Decode.andThen (mergePasses dimensions >> Decode.succeed)


image : PixelInfo -> Dimensions -> Decoder Image
image pixelInfo ({ height } as dimensions) =
  let
      pixels ln acc =
        PixelArray.initialize pixelInfo ln |> Array.append acc

      process =
        Filter.revert pixelInfo
          >> List.foldr pixels Array.empty
          >> Image.fromArray dimensions
          >> Decode.succeed
  in
  list height (line dimensions pixelInfo)
    |> Decode.andThen process


line : Dimensions -> PixelInfo -> Decoder (Filter, List Int)
line { width } pixelInfo =
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
