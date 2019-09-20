module Matrix exposing
  ( Matrix
  , Dimensions
  , repeat
  , fromArray
  , toArray
  , get
  , set
  , dimensions
  , positionFromIndex
  , foldLines
  )


import Array exposing (Array)


type alias Dimensions =
  { width: Int
  , height: Int
  }


type Matrix a =
  Matrix Dimensions (Array a)


repeat : Dimensions -> a -> Matrix a
repeat ({ width, height } as dims) a =
  Matrix dims <| Array.repeat (width * height) a


fromArray : Dimensions -> Array a -> Matrix a
fromArray ({ width, height } as dims) array =
  Array.slice 0 (width * height) array
    |> Matrix dims


toArray : Matrix a -> (Array a)
toArray (Matrix _ array) =
  array


dimensions : Matrix a -> Dimensions
dimensions (Matrix dims _) =
  dims


get : (Int, Int) -> Matrix a -> Maybe a
get ( x, y ) (Matrix { width } array) =
  Array.get (y * width + x) array


set : (Int, Int) -> a -> Matrix a -> Matrix a
set ( x, y ) pixel (Matrix ({ width } as dims) array) =
  Array.set (y * width + x) pixel array
    |> fromArray dims


positionFromIndex : Dimensions -> Int -> ( Int, Int )
positionFromIndex { width } idx =
  ( remainderBy width idx, idx // width )


foldLines : (Array a -> b) -> Matrix a -> List b
foldLines fun (Matrix { width } array) =
  foldLinesHelp fun width array []
    |> List.reverse


foldLinesHelp : (Array a -> b) -> Int -> Array a -> List b -> List b
foldLinesHelp fun width array acc =
  let
      length = Array.length array
      slice = Array.slice 0 width array
      rest = Array.slice width length array
  in
  if length > 0 then
    foldLinesHelp fun width rest <| fun slice :: acc
  else
    acc
