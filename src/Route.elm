module Route exposing (FilterQueryParams, Route(..), addFilterParams, fromUrl, parser, transformParams)

import Api
import Browser.Navigation as Nav
import Graphql.OptionalArgument as GqlOptional
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


filterQueryParams : Query.Parser FilterQueryParams
filterQueryParams =
    Query.map2 FilterQueryParams (Query.string "search") (Query.string "genre")


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url


addFilterParams : Nav.Key -> FilterQueryParams -> Cmd msg
addFilterParams key params =
    Nav.replaceUrl key <| Builder.relative [] (parseParams params)



-- only append parameters that are present values


parseParams : FilterQueryParams -> List Builder.QueryParameter
parseParams { search, genre } =
    Maybe.Extra.values [ Maybe.map (Builder.string "search") search, Maybe.map (Builder.string "genre") genre ]



-- transform Maybe params into the Graphql.OptionalArgument type (Just A -> Present A, Nothing -> Absent)


transformParams : FilterQueryParams -> Api.Filter
transformParams filterQueryPrams =
    { search = GqlOptional.fromMaybe filterQueryPrams.search, genre = GqlOptional.fromMaybe filterQueryPrams.genre }
