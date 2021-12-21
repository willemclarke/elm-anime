module Route exposing (FilterQueryParams, Route(..), addFilterParams, fromUrl, parser, transformParams)

import AniList.Enum.MediaSort exposing (MediaSort)
import AniList.Enum.MediaType
import AniList.Object exposing (Media)
import Api
import Browser.Navigation as Nav
import Graphql.Internal.Builder.Argument exposing (Argument)
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
    , mediaType : Maybe String
    , genre : Maybe String
    , sort : Maybe String
    }


parser : Url.Parser.Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Home <| Url.Parser.top <?> filterQueryParams ]


filterQueryParams : Query.Parser FilterQueryParams
filterQueryParams =
    Query.map4 FilterQueryParams (Query.string "search") (Query.string "type") (Query.string "genre") (Query.string "sort")


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url


addFilterParams : Nav.Key -> FilterQueryParams -> Cmd msg
addFilterParams key params =
    Nav.replaceUrl key <| Builder.relative [] (parseParams params)



-- only append parameters that are present values


parseParams : FilterQueryParams -> List Builder.QueryParameter
parseParams { search, mediaType, genre, sort } =
    Maybe.Extra.values
        [ Maybe.map (Builder.string "search") search
        , Maybe.map (Builder.string "type") mediaType
        , Maybe.map (Builder.string "genre") genre
        , Maybe.map (Builder.string "sort") sort
        ]



-- transform Maybe params into the Graphql.OptionalArgument type (Just A -> Present A, Nothing -> Absent)


transformParams : FilterQueryParams -> Api.Filter
transformParams { search, mediaType, genre, sort } =
    { search = GqlOptional.fromMaybe search
    , mediaType = GqlOptional.fromMaybe (fromMaybeStringToMediaType mediaType)
    , genre = GqlOptional.fromMaybe genre
    , sort = GqlOptional.fromMaybe (Just [ fromMaybeStringToMediaSort sort ])
    }



-- turn a Maybe String into an MediaSort enum that the optional arguments can recognise


fromMaybeStringToMediaType : Maybe String -> Maybe AniList.Enum.MediaType.MediaType
fromMaybeStringToMediaType mediaType =
    case mediaType of
        Just mt ->
            AniList.Enum.MediaType.fromString mt

        Nothing ->
            AniList.Enum.MediaType.fromString "ANIME"


fromMaybeStringToMediaSort : Maybe String -> Maybe AniList.Enum.MediaSort.MediaSort
fromMaybeStringToMediaSort string =
    case string of
        Just s ->
            AniList.Enum.MediaSort.fromString s

        Nothing ->
            AniList.Enum.MediaSort.fromString "SCORE_DESC"
