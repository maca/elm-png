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
fromArray dims array =
  Matrix dims array


toArray : Matrix a -> (Array a)
toArray (Matrix _ array) =
  array


dimensions : Matrix a -> Dimensions
dimensions (Matrix dims _) =
  dims


get : (Int, Int) -> Matrix a -> Maybe a
get ( x, y ) (Matrix { width } ps) =
  Array.get (y * width + x) ps


set : (Int, Int) -> a -> Matrix a -> Matrix a
set ( x, y ) pixel (Matrix ({ width } as dims) ps) =
  Array.set (y * width + x) pixel ps
    |> fromArray dims


positionFromIndex : Dimensions -> Int -> ( Int, Int )
positionFromIndex { width } idx =
  ( remainderBy width idx, idx // width )
