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
import Html.Attributes exposing (class, href, placeholder, src, type_)
import Html.Events exposing (onInput, onSubmit)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )
import Maybe exposing (withDefault)
import Maybe.Extra exposing (or)
import RemoteData exposing (RemoteData)



---- MODEL ----


type alias Model =
    { data : RemoteData (Graphql.Http.Error Response) Response, searchTerm : String }


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
    ( { data = RemoteData.Loading, searchTerm = "" }, makeRequest Nothing )



-- Gql API related functions


query : Maybe String -> SelectionSet (Maybe Page) RootQuery
query searchTerm =
    Query.page (\optionals -> { optionals | page = Present 1, perPage = Present 100 }) (pageSelection searchTerm)


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


makeRequest : Maybe String -> Cmd Msg
makeRequest searchTerm =
    searchTerm
        |> query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



---- UPDATE ----


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | ChangeInput String
    | Refetch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( { model | data = response }, Cmd.none )

        ChangeInput newInput ->
            ( { model | searchTerm = newInput }, Cmd.none )

        Refetch ->
            ( { model | data = RemoteData.Loading }, makeRequest (Just model.searchTerm) )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        children =
            case model.data of
                RemoteData.Loading ->
                    loadingSpinner

                RemoteData.NotAsked ->
                    text "not asked is true"

                RemoteData.Failure _ ->
                    text "unable to fetch genres"

                RemoteData.Success response ->
                    case response of
                        Just page ->
                            div []
                                [ siteTitle
                                , filters
                                , displayMangaList
                                    (sanitizeMangaList page.manga)
                                ]

                        Nothing ->
                            text "No genres found."
    in
    baseLayout children



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--- MAIN ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



-- View functions


baseLayout : Html Msg -> Html Msg
baseLayout children =
    div [ class "flex justify-center h-full bg-gray-100 mt-6" ]
        [ children ]


siteTitle : Html Msg
siteTitle =
    h1 [ class "text-center mt-2 text-3xl 2xl:text-4xl filter drop-shadow-sm font-extrabold text-transparent bg-clip-text bg-gradient-to-r from-blue-900 to-blue-400" ] [ text "elm-manga" ]


filters : Html Msg
filters =
    div [ class "flex justify-start mt-10 mx-16" ] [ searchFilter ]


searchFilter : Html Msg
searchFilter =
    div []
        [ div [ class "text-gray-700 " ] [ text "Search" ]
        , div []
            [ form [ onSubmit Refetch ]
                [ input [ class "mt-1 p-2 rounded shadow-l text-gray-700 ", placeholder "Search manga", type_ "search", onInput ChangeInput ] []
                ]
            ]
        ]


loadingSpinner : Html Msg
loadingSpinner =
    div [ class "flex items-center h-full" ]
        [ Loading.render
            Circle
            { defaultConfig | color = "#333" }
            Loading.On
        ]


displayMangaList : List Manga -> Html Msg
displayMangaList mangaList =
    div [ class "mx-16 mt-8 mb-16 grid grid-cols-1 sm:grid-cols-2 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-5 gap-6" ]
        (List.map displayManga mangaList)


displayManga : Manga -> Html Msg
displayManga manga =
    a [ href ("https://anilist.co/manga/" ++ String.fromInt manga.id) ]
        [ div [ class "w-48 h-80 text-center text-gray-700 bg-white rounded overflow-hidden shadow-lg hover:text-indigo-900 hover:shadow-2xl" ]
            [ img [ src (sanitizeCoverImage manga.coverImage), class "h-64 w-full" ]
                []
            , div
                []
                [ p [ class "text-l font-bold hover:font-black truncate mx-2 mt-1 mb-1" ] [ text (sanitizeTitle manga.title) ]
                , displayGenres (sanitizeGenres manga.genres)
                ]
            ]
        ]


displayGenres : List String -> Html Msg
displayGenres genres =
    let
        firstTwoGenres =
            List.take 2 genres
    in
    div [ class "mx-2" ]
        (List.map
            (\genre -> span [ class "inline-block bg-blue-200 rounded-full px-2 text-xs font-semibold text-gray-700 mr-1 mb-1" ] [ text genre ])
            firstTwoGenres
        )



-- API helpers, predominately dealing with `Maybes`


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
