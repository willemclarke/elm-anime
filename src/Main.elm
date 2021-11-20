module Main exposing (..)

import AniList.Enum.MediaSort
import AniList.Enum.MediaType
import AniList.Object
import AniList.Object.Media as Media
import AniList.Object.MediaCoverImage as CoverImage
import AniList.Object.MediaTitle as MediaTitle
import AniList.Object.Page as Page
import AniList.Query as Query
import Browser
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Html.Attributes exposing (class, height, href, placeholder, src, style, value, width)
import Html.Events exposing (onMouseOver)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )
import Maybe exposing (withDefault)
import Maybe.Extra exposing (or)
import Process exposing (Id)
import RemoteData exposing (RemoteData)



---- MODEL ----


type Msg
    = GotResponse Model
    | ShowExtraInfo


type alias Model =
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading, makeRequest )


query : SelectionSet (Maybe Page) RootQuery
query =
    Query.page (\optionals -> { optionals | page = Present 1, perPage = Present 100 }) pageSelection



-- try add second optional argument for enum to `SORT_DESC`'


pageSelection : SelectionSet Page AniList.Object.Page
pageSelection =
    SelectionSet.map Page (Page.media (\optionals -> { optionals | type_ = Present AniList.Enum.MediaType.Manga, sort = Present [ Just AniList.Enum.MediaSort.ScoreDesc ] }) mediaSelection)


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


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( response, Cmd.none )

        ShowExtraInfo ->
            ( model, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        children =
            case model of
                RemoteData.Loading ->
                    loadingSpinner

                RemoteData.NotAsked ->
                    text "not asked is true"

                RemoteData.Failure _ ->
                    text "unable to fetch genres"

                RemoteData.Success response ->
                    case response of
                        Just page ->
                            displayMangaList (sanitizeMangaList page.manga)

                        Nothing ->
                            text "No genres found."
    in
    baseLayout children


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


baseLayout : Html Msg -> Html Msg
baseLayout children =
    div [ class "flex justify-center h-100vh bg-gray-100 mt-6" ] [ children ]


loadingSpinner : Html Msg
loadingSpinner =
    div []
        [ Loading.render
            Circle
            { defaultConfig | color = "#333" }
            Loading.On
        ]


displayMangaList : List Manga -> Html Msg
displayMangaList mangaList =
    div [ class "p-16 grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 gap-5" ]
        (List.map displayManga mangaList)



-- best card height = h-80, for now using h-96, revert eventually


displayManga : Manga -> Html Msg
displayManga manga =
    a [ href ("https://anilist.co/manga/" ++ String.fromInt manga.id) ]
        [ div [ class "w-48 h-96 text-center text-gray-700 bg-white rounded overflow-hidden shadow-lg hover:shadow-2xl hover:text-indigo-900" ]
            [ img [ src (sanitizeCoverImage manga.coverImage), class "h-72 w-full" ]
                []
            , div
                []
                [ p [ class "text-l font-bold truncate mt-1 " ] [ text (sanitizeTitle manga.title) ]
                ]
            , displayGenres (sanitizeGenres manga.genres)
            ]
        ]


displayGenres : List String -> Html Msg
displayGenres genres =
    div [ class "px-2 pt-2" ]
        (List.map
            (\genre -> span [ class "inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 mb-2" ] [ text ("#" ++ genre) ])
            genres
        )


sanitizeMangaList : Maybe (List (Maybe Manga)) -> List Manga
sanitizeMangaList mangaList =
    case mangaList of
        Just list ->
            List.filterMap identity list

        Nothing ->
            []


sanitizeGenres : Maybe (List (Maybe String)) -> List String
sanitizeGenres genreList =
    case genreList of
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


sanitizeAverageScore : Maybe Int -> String
sanitizeAverageScore score =
    case score of
        Just s ->
            String.fromInt s

        Nothing ->
            "No score"
