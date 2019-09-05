module Png exposing (..)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing
    (Decoder, Step(..), decode, unsignedInt8)
import Bytes.Encode as Encode exposing (Encoder)


type Png = Png


signature : List Int
signature =
  [ 137, 80, 78, 71, 13, 10, 26, 10 ]


signatureDecoder : Decoder (List Int)
signatureDecoder =
  Decode.loop (List.length signature, []) (listStep unsignedInt8)


listStep : Decoder a -> (Int, List a)
                     -> Decoder (Step (Int, List a) (List a))
listStep decoder (n, xs) =
  if n <= 0 then
    Decode.succeed (Done <| List.reverse xs)
  else
    Decode.map (\x -> Loop (n - 1, x :: xs)) decoder


decodePng bytes =
  decode signatureDecoder bytes
