module Route exposing (Route(..), fromUrl, parser, setQueryParam)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Builder exposing (Root(..))
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


type Route
    = Home (Maybe String)
    | NotFound


parser : Url.Parser.Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Home <| Url.Parser.top <?> Url.Parser.Query.string "search" ]


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url


setQueryParam : Nav.Key -> String -> Cmd msg
setQueryParam key queryString =
    Nav.replaceUrl key <| Url.Builder.relative [ "/" ] [ Url.Builder.string "search" queryString ]
