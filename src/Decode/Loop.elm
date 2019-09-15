module Decode.Loop exposing (..)


import Bytes.Decode as Decode exposing (Decoder, Step(..))


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


iterStep : (a -> Decoder b) -> (List a, List b)
                            -> Decoder (Step (List a, List b) (List b))
iterStep decoder (ys, xs) =
  case ys of
    [] ->
      Decode.succeed (Done <| List.reverse xs)

    y :: rest ->
      Decode.map (\x -> Loop (rest, x :: xs)) (decoder y)
