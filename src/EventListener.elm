module EventListener exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Json.Decode as Decode


type alias Model =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( "-", Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text model
        ]


type Msg
    = KeyPressed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed value ->
            ( model ++ value, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyPress (Decode.map KeyPressed <| Decode.field "key" Decode.string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
