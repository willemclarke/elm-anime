module Route exposing (FilterQueryParams, Route(..), fromUrl, parser, setFilterParams)

import Browser.Navigation as Nav
import Maybe.Extra
import Url
import Url.Builder as Builder
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


type Route
    = Home FilterQueryParams


type alias FilterQueryParams =
    { search : Maybe String
    , genre : Maybe String
    }


parser : Url.Parser.Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Home <| Url.Parser.top <?> filterQueryParams ]


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url


filterQueryParams : Query.Parser FilterQueryParams
filterQueryParams =
    Query.map2 FilterQueryParams (Query.string "search") (Query.string "genre")


setFilterParams : Nav.Key -> FilterQueryParams -> Cmd msg
setFilterParams key params =
    Nav.replaceUrl key <| Builder.relative [] (parseParams params)


parseParams : FilterQueryParams -> List Builder.QueryParameter
parseParams { search, genre } =
    Maybe.Extra.values [ Maybe.map (Builder.string "search") search, Maybe.map (Builder.string "genre") genre ]
