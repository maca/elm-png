module Adam7 exposing (passDimensions, mergePasses)


import Array
import Bitwise exposing (shiftLeftBy, shiftRightBy)


import Image exposing (Image)
import Matrix exposing (Dimensions, positionFromIndex)
import Pixel exposing (Pixel)


type alias Offset =
    { wShift : Int
    , wOffset : Int
    , hShift : Int
    , hOffset : Int
    }


passDimensions : Int -> Dimensions -> Dimensions
passDimensions pass { width, height } =
  case passOffset pass of
    Just { wShift, wOffset, hShift, hOffset } ->
      { width = bitShift width wShift wOffset
      , height = bitShift height hShift hOffset
      }

    Nothing ->
      { width = 0, height = 0 }


passOffset : Int -> Maybe Offset
passOffset pass =
  case pass of
    0 -> Just <| Offset 3 0 3 0
    1 -> Just <| Offset 3 4 3 0
    2 -> Just <| Offset 2 0 3 4
    3 -> Just <| Offset 2 2 2 0
    4 -> Just <| Offset 1 0 2 2
    5 -> Just <| Offset 1 1 1 0
    6 -> Just <| Offset 0 0 1 1
    _ -> Nothing


bitShift : Int -> Int -> Int -> Int
bitShift length shift offset =
  shiftRightBy shift
    (length - offset - 1 + (shiftLeftBy shift 1))


mergePasses : Dimensions -> List Image -> Image
mergePasses dimensions passes =
  passes
    |> List.foldl mergePass ( 0, Image.blank dimensions )
    |> Tuple.second


mergePass : Image -> ( Int, Image ) -> ( Int, Image )
mergePass pass ( passNum, img ) =
  let
      dimensions =
        Image.dimensions pass

      offset =
        passOffset passNum

      ( _, newImg ) =
        Image.toArray pass
          |> Array.foldl (setPixel dimensions offset) ( 0, img )
  in
    ( passNum + 1, newImg )


setPixel : Dimensions -> Maybe Offset
                      -> Pixel
                      -> ( Int, Image )
                      -> ( Int, Image )
setPixel dimensions maybe pixel ( idx, img ) =
  case maybe of
    Just offset ->
      let
        pos =
          positionFromIndex dimensions idx
            |> offsetPosition offset
      in
      ( idx + 1, Image.set pos pixel img )

    Nothing ->
      ( idx + 1, img )


offsetPosition : Offset -> ( Int, Int ) -> ( Int, Int )
offsetPosition { wShift, wOffset, hShift, hOffset } ( x, y ) =
  ( Bitwise.or (shiftLeftBy wShift x) wOffset
  , Bitwise.or (shiftLeftBy hShift y) hOffset
  )
