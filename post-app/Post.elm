module Post exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as Encode
import Ports
import Url.Parser exposing (Parser, custom)


type alias Post =
    { id : PostId
    , title : String
    , authorName : String
    , authorUrl : String
    }


postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" idDecoder
        |> required "title" string
        |> requiredAt [ "author", "name" ] string
        |> requiredAt [ "author", "url" ] string


type PostId
    = PostId Int


idDecoder : Decoder PostId
idDecoder =
    Decode.map PostId int


idToString : PostId -> String
idToString (PostId id) =
    String.fromInt id


idParser : Parser (PostId -> a) a
idParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map PostId (String.toInt postId)


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", encodeId post.id )
        , ( "title", Encode.string post.title )
        , ( "author", postAuthorEncoder post.authorName post.authorUrl )
        ]


postAuthorEncoder : String -> String -> Encode.Value
postAuthorEncoder authorName authorUrl =
    Encode.object
        [ ( "name", Encode.string authorName )
        , ( "url", Encode.string authorUrl )
        ]


encodeId : PostId -> Encode.Value
encodeId (PostId id) =
    Encode.int id


emptyPost : Post
emptyPost =
    { id = PostId -1
    , title = ""
    , authorName = ""
    , authorUrl = ""
    }


newPostEncoder : Post -> Encode.Value
newPostEncoder post =
    Encode.object
        [ ( "title", Encode.string post.title )
        , ( "author", postAuthorEncoder post.authorName post.authorUrl )
        ]


savePosts : List Post -> Cmd msg
savePosts posts =
    Encode.list postEncoder posts
        |> Encode.encode 0
        |> Ports.storePosts
