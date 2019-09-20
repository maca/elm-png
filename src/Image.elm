module Image exposing
  ( Image
  , fromArray
  , toArray
  , blank
  , dimensions
  , get
  , set
  , foldLines
  )


import Array exposing (Array)
import Matrix exposing (Matrix, Dimensions)
import Pixel exposing (Pixel, blank)
import Bytes exposing (Bytes)
import Bytes.Encode exposing (Encoder, encode, sequence, unsignedInt8)


type Image
  = Image (Matrix Pixel)


fromArray : Dimensions -> Array Pixel -> Image
fromArray dims array =
  Matrix.fromArray dims array |> Image


toArray : Image -> Array Pixel
toArray image =
  toMatrix image |> Matrix.toArray


blank : Dimensions -> Image
blank dims =
  Matrix.repeat dims Pixel.blank |> Image


dimensions : Image -> Dimensions
dimensions (Image matrix) =
  Matrix.dimensions matrix


get : (Int, Int) -> Image -> Maybe Pixel
get position image =
  toMatrix image |> Matrix.get position


set : (Int, Int) -> Pixel -> Image -> Image
set position pixel image =
  toMatrix image |> Matrix.set position pixel |> Image


toMatrix : Image -> Matrix Pixel
toMatrix (Image matrix) =
  matrix


foldLines : (Array Pixel -> b) -> Image -> List b
foldLines fun (Image matrix) =
  Matrix.foldLines fun matrix
