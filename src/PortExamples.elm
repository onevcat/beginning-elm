port module PortExamples exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    String


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendDataToJS ] [ text "Send Data to JavaScript" ]
        , br [] []
        , br [] []
        , text ("Data received from JavaScript: " ++ model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendDataToJS ->
            ( model, sendData "Hello JavaScript!" )

        ReceivedDataFromJS value ->
            ( value, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )


type Msg
    = SendDataToJS
    | ReceivedDataFromJS Model


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port sendData : String -> Cmd msg


port receivedData : (Model -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    receivedData ReceivedDataFromJS
