module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode exposing (decodeString)
import Page.EditPost as EditPost
import Page.ListPosts as ListPosts
import Page.NewPost as NewPost
import Post exposing (Post, postsDecoder)
import RemoteData exposing (WebData)
import Route exposing (Route)
import Url exposing (Url)


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | ListPage ListPosts.Model
    | EditPage EditPost.Model
    | NewPage NewPost.Model


init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }

        posts =
            case flags of
                Just postsJson ->
                    decodeStoredPosts postsJson

                Nothing ->
                    RemoteData.Loading
    in
    initCurrentPage posts ( model, Cmd.none )


decodeStoredPosts : String -> WebData (List Post)
decodeStoredPosts postsJson =
    case decodeString postsDecoder postsJson of
        Ok posts ->
            RemoteData.succeed posts

        Err _ ->
            RemoteData.Loading


initCurrentPage : WebData (List Post) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage posts ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Posts ->
                    let
                        ( pageModel, pageCmds ) =
                            ListPosts.init posts
                    in
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )

                Route.Post postId ->
                    let
                        ( pageModel, pageCmds ) =
                            EditPost.init postId model.navKey
                    in
                    ( EditPage pageModel, Cmd.map EditPageMsg pageCmds )

                Route.NewPost ->
                    let
                        ( pageModel, pageCmds ) =
                            NewPost.init model.navKey
                    in
                    ( NewPage pageModel, Cmd.map NewPageMsg pageCmds )
    in
    ( { model | page = currentPage }, Cmd.batch [ existingCmds, mappedPageCmds ] )


type Msg
    = ListPageMsg ListPosts.Msg
    | EditPageMsg EditPost.Msg
    | NewPageMsg NewPost.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url


view : Model -> Document Msg
view model =
    { title = "Post App"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        ListPage pageModel ->
            ListPosts.view pageModel |> Html.map ListPageMsg

        EditPage pageModel ->
            EditPost.view pageModel |> Html.map EditPageMsg

        NewPage pageModel ->
            NewPost.view pageModel |> Html.map NewPageMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedModel, updatedCmd ) =
                    ListPosts.update subMsg pageModel
            in
            ( { model | page = ListPage updatedModel }
            , Cmd.map ListPageMsg updatedCmd
            )

        ( EditPageMsg subMsg, EditPage pageModel ) ->
            let
                ( updatedModel, updatedCmd ) =
                    EditPost.update subMsg pageModel
            in
            ( { model | page = EditPage updatedModel }
            , Cmd.map EditPageMsg updatedCmd
            )

        ( NewPageMsg subMsg, NewPage pageModel ) ->
            let
                ( updatedModel, updatedCmd ) =
                    NewPost.update subMsg pageModel
            in
            ( { model | page = NewPage updatedModel }
            , Cmd.map NewPageMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none ) |> initCurrentPage RemoteData.Loading

        ( _, _ ) ->
            ( model, Cmd.none )


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]
