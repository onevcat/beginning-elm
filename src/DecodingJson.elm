module DecodingJson exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData(..), WebData)


type alias Post =
    { id : Int
    , title : String
    , author : String
    }


type alias Model =
    { posts : WebData (List Post)
    }


type Msg
    = FetchPosts
    | DataReceived (WebData (List Post))


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchPosts ] [ text "Refresh Posts" ]
        , viewPostsOrError model
        ]


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.posts of
        RemoteData.Failure error ->
            viewError (buildErrorMessage error)

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success posts ->
            viewPosts posts


viewError : String -> Html Msg
viewError message =
    div []
        [ h3 [] [ text "Couldn't fetch data at this time" ]
        , text ("Error: " ++ message)
        ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div []
        [ h3 [] [ text "Posts" ]
        , table []
            ([ viewTableHeader ] ++ List.map viewPost posts)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "Title" ]
        , th []
            [ text "Author" ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    tr []
        [ td []
            [ text (String.fromInt post.id) ]
        , td []
            [ text post.title ]
        , td []
            [ text post.author ]
        ]


url : String
url =
    "http://localhost:5019/posts"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( { model | posts = RemoteData.Loading }
            , httpCommand
            )

        DataReceived data ->
            ( { model | posts = data }
            , Cmd.none
            )


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = url
        , expect =
            list postDecoder
                |> Http.expectJson (RemoteData.fromResult >> DataReceived)
        }


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


postDecoder : Decoder Post
postDecoder =
    succeed Post
        |> required "id" int
        |> required "title" string
        |> required "author" string


postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = RemoteData.Loading }, httpCommand )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
