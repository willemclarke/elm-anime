module API.Api exposing (Manga, MangaData, query, sanitizeAverageScore, sanitizeCoverImage, sanitizeGenres, sanitizeMangaList, sanitizeTitle)

import AniList.Enum.MediaSort
import AniList.Enum.MediaType
import AniList.Object
import AniList.Object.Media as Media
import AniList.Object.MediaCoverImage as CoverImage
import AniList.Object.MediaExternalLink exposing (url)
import AniList.Object.MediaTitle as MediaTitle
import AniList.Object.Page as Page
import AniList.Query as Query
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Maybe.Extra exposing (or)
import RemoteData exposing (RemoteData)


type alias MangaData =
    RemoteData (Graphql.Http.Error Response) Response


type alias Response =
    Maybe Page


type alias Page =
    { manga : Maybe (List (Maybe Manga)) }


type alias Manga =
    { id : Int, averageScore : Maybe Int, title : Maybe Title, coverImage : Maybe CoverImage, genres : Maybe (List (Maybe String)) }


type alias Title =
    { romaji : Maybe String, english : Maybe String }


type alias CoverImage =
    { large : Maybe String }


query : Maybe String -> SelectionSet (Maybe Page) RootQuery
query searchTerm =
    Query.page (\optionals -> { optionals | page = Present 1, perPage = Present 100 }) (pageSelection searchTerm)



-- functions to handle Maybe's, which this api's schema was rife with


pageSelection : Maybe String -> SelectionSet Page AniList.Object.Page
pageSelection searchTerm =
    SelectionSet.map Page (Page.media (\optionals -> { optionals | type_ = Present AniList.Enum.MediaType.Manga, sort = Present [ Just AniList.Enum.MediaSort.ScoreDesc ], isAdult = Present False, search = fromMaybe searchTerm }) mediaSelection)


mediaSelection : SelectionSet Manga AniList.Object.Media
mediaSelection =
    SelectionSet.map5 Manga
        Media.id
        Media.averageScore
        (Media.title titleSelection)
        (Media.coverImage coverImageSelection)
        Media.genres


titleSelection : SelectionSet Title AniList.Object.MediaTitle
titleSelection =
    SelectionSet.map2 Title
        (MediaTitle.english identity)
        (MediaTitle.romaji identity)


coverImageSelection : SelectionSet CoverImage AniList.Object.MediaCoverImage
coverImageSelection =
    SelectionSet.map CoverImage
        CoverImage.large


sanitizeMangaList : Maybe (List (Maybe Manga)) -> List Manga
sanitizeMangaList mangaList =
    case mangaList of
        Just list ->
            List.filterMap identity list

        Nothing ->
            []


sanitizeTitle : Maybe Title -> String
sanitizeTitle title =
    case title of
        Just { romaji, english } ->
            Maybe.withDefault "No title found" (or romaji english)

        Nothing ->
            "No title found"


sanitizeCoverImage : Maybe CoverImage -> String
sanitizeCoverImage image =
    case image of
        Just { large } ->
            Maybe.withDefault "No image found" large

        Nothing ->
            "No image found"


sanitizeGenres : Maybe (List (Maybe String)) -> List String
sanitizeGenres genreList =
    case genreList of
        Just list ->
            List.filterMap identity list

        Nothing ->
            []


sanitizeAverageScore : Maybe Int -> String
sanitizeAverageScore score =
    case score of
        Just s ->
            String.fromInt s

        Nothing ->
            "No score"
