module Route exposing (FilterQueryParams, Route(..), fromUrl, parser, setGenreParam, setSearchParam)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Builder exposing (Root(..))
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


type Route
    = Home FilterQueryParams


type alias FilterQueryParams =
    { search : Maybe String
    , genre : Maybe String
    }


parser : Url.Parser.Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Home <| Url.Parser.top <?> queryParams ]


queryParams : Url.Parser.Query.Parser FilterQueryParams
queryParams =
    Url.Parser.Query.map2 FilterQueryParams (Url.Parser.Query.string "search") (Url.Parser.Query.string "genre")


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url


setSearchParam : Nav.Key -> String -> Cmd msg
setSearchParam key searchTerm =
    Nav.replaceUrl key <| Url.Builder.relative [] [ Url.Builder.string "search" searchTerm ]


setGenreParam : Nav.Key -> String -> Cmd msg
setGenreParam key genre =
    Nav.replaceUrl key <| Url.Builder.relative [] [ Url.Builder.string "genre" genre ]
