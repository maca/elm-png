module Png.Signature exposing (signature, encoder, decoder)

import Decode.Loop exposing (list)


import Bytes.Decode as Decode exposing (Decoder, unsignedInt8)
import Bytes.Encode as Encode exposing (Encoder, sequence)


signature : List Int
signature =
  [ 137, 80, 78, 71, 13, 10, 26, 10 ]


encoder : Encoder
encoder =
  Encode.sequence <| List.map Encode.unsignedInt8 signature


decoder : Decoder (List Int)
decoder =
  list (List.length signature) unsignedInt8
    |> Decode.andThen
        (\s -> if s == signature then Decode.succeed s else Decode.fail)
