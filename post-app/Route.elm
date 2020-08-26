module Route exposing (..)

import Browser.Navigation as Nav
import Post exposing (PostId)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Posts
    | Post PostId
    | NewPost


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Posts top
        , map Posts (s "posts")
        , map Post (s "posts" </> Post.idParser)
        , map NewPost (s "posts" </> s "new")
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Posts ->
            "/posts"

        Post postId ->
            "/posts/" ++ Post.idToString postId

        NewPost ->
            "/posts/new"
