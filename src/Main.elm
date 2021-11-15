module Main exposing (..)

-- import AniList.Enum.MediaType
-- import Graphql.Document as Document
-- import Graphql.Http.GraphqlError

import AniList.Enum.MediaType
import AniList.Object
import AniList.Object.Media as Media
import AniList.Object.MediaTitle as MediaTitle
import AniList.Object.Page as Page
import AniList.Query as Query
import Browser
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Html.Attributes exposing (class, height, placeholder, src, style, value, width)
import Html.Events exposing (..)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )
import Maybe exposing (withDefault)
import Process exposing (Id)
import RemoteData exposing (RemoteData)



---- MODEL ----


type Msg
    = GotResponse Model


type alias Model =
    RemoteData (Graphql.Http.Error Response) Response


type alias Response =
    Maybe Page


type alias Page =
    { manga : Maybe (List (Maybe Manga)) }


type alias Manga =
    { id : Int, averageScore : Maybe Int, title : Maybe Title }


type alias Title =
    { romaji : Maybe String, english : Maybe String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading, makeRequest )


query : SelectionSet (Maybe Page) RootQuery
query =
    Query.page (\optionals -> { optionals | page = Present 1, perPage = Present 10 }) pageSelection



-- where `identity` replace with optional arguments to only dispaly 'manga'


pageSelection : SelectionSet Page AniList.Object.Page
pageSelection =
    SelectionSet.map Page (Page.media (\optionals -> { optionals | type_ = Present AniList.Enum.MediaType.Manga }) mediaSelection)


mediaSelection : SelectionSet Manga AniList.Object.Media
mediaSelection =
    SelectionSet.map3 Manga
        Media.id
        Media.averageScore
        (Media.title titleSelection)


titleSelection : SelectionSet Title AniList.Object.MediaTitle
titleSelection =
    SelectionSet.map2 Title
        (MediaTitle.english identity)
        (MediaTitle.romaji identity)


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
    div [ class "flex justify-center h-full bg-grey-100 mt-6" ] [ children ]


loadingSpinner : Html Msg
loadingSpinner =
    div []
        [ Loading.render
            Circle
            { defaultConfig | color = "#333" }
            Loading.On
        ]


sanitizeMangaList : Maybe (List (Maybe Manga)) -> List Manga
sanitizeMangaList mangaList =
    case mangaList of
        Just list ->
            List.filterMap identity list

        Nothing ->
            []


displayMangaList : List Manga -> Html Msg
displayMangaList mangaList =
    div [ class "flex justify-center h-96 bg-grey-500" ]
        (List.map
            (\manga -> displayManga manga)
            mangaList
        )


sanitizeTitle : Maybe Title -> Title
sanitizeTitle title =
    case title of
        Just t ->
            t

        Nothing ->
            { romaji = Nothing, english = Nothing }


sanitizeAverageScore : Maybe Int -> String
sanitizeAverageScore score =
    case score of
        Just s ->
            String.fromInt s

        Nothing ->
            "No score"


displayManga : Manga -> Html Msg
displayManga manga =
    let
        sanitizedTitle =
            sanitizeTitle manga.title
    in
    div [ class "h-4 m-4 w-24" ]
        [ p [ class "text-xl" ] [ text (Maybe.withDefault "No Title found" sanitizedTitle.romaji) ]
        , p [ class "text-base" ] [ text ("Score: " ++ sanitizeAverageScore manga.averageScore) ]
        , p [ class "text-base" ] [ text ("Id: " ++ String.fromInt manga.id) ]
        ]
