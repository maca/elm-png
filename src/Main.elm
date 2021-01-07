import Browser
import Bytes exposing (Bytes, Endianness(..))
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Task


import File.Download as Download

import Png exposing (..)



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { hover : Bool
  , bytes : List Bytes
  , url : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  (Model False [] "", Cmd.none)



-- UPDATE


type Msg
  = Pick
  | DragEnter
  | DragLeave
  | GotFiles File (List File)
  | GotBytes (List Bytes)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pick ->
      ( model
      , Select.files ["image/*"] GotFiles
      )

    DragEnter ->
      ( { model | hover = True }
      , Cmd.none
      )

    DragLeave ->
      ( { model | hover = False }
      , Cmd.none
      )

    GotFiles file files ->
      ( { model | hover = False }
      , Task.perform GotBytes
          <| Task.sequence
          <| List.map File.toBytes (file :: files)
      )

    GotBytes [bytes] ->
      let
          _ = Png.fromBytes bytes
              |> Debug.log "generating image"
              |> Maybe.map Png.toImage
              |> Debug.log "image"
            -- url =
            --   Png.toImage png
            --     |> Maybe.map (Png.fromImage)
            --     -- |> Maybe.withDefault ""

      in
      ( model, Cmd.none )
      -- case Png.fromBytes bytes of
      --   Just png ->
      --     let
      --       -- _ = Debug.log "originalBytes" bytes
      --       -- _ = Debug.log "image" <| Png.toImage png
      --       -- _ = Debug.log "image2"
      --       --     <| Maybe.map (Png.fromImage >> Png.toImage)
      --       --     <| Png.toImage png

      --       url =
      --         Png.toImage png
      --           |> Maybe.map (Png.fromImage >> Png.toUrl)
      --           |> Maybe.withDefault ""

      --     in
      --     ( { model | url = url }, Cmd.none )

      --   Nothing ->
      --     ( model, Cmd.none )

    GotBytes _ ->
      ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style "border" (if model.hover then "6px dashed purple" else "6px dashed #ccc")
    , style "border-radius" "20px"
    , style "width" "480px"
    , style "margin" "100px auto"
    , style "padding" "40px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "align-items" "center"
    , hijackOn "dragenter" (Decode.succeed DragEnter)
    , hijackOn "dragover" (Decode.succeed DragEnter)
    , hijackOn "dragleave" (Decode.succeed DragLeave)
    , hijackOn "drop" dropDecoder
    ]
    [ button [ onClick Pick ] [ text "Upload Images" ]
    , img [ src model.url ] [ ]
    ]


-- viewPreview : String -> Html msg
-- viewPreview url =
--   div
--     [ style "background-image" ("url('" ++ url ++ "')")
--     , style "background-position" "center"
--     , style "background-repeat" "no-repeat"
--     , style "background-size" "contain"
--     ]
--     []


dropDecoder : Decode.Decoder Msg
dropDecoder =
  Decode.at
    ["dataTransfer","files"]
    (Decode.oneOrMore GotFiles File.decoder)


hijackOn : String -> Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> (msg, Bool)
hijack msg =
  (msg, True)
