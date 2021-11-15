module Main exposing (..)

-- import AniList.Enum.MediaType
-- import Browser
-- import Graphql.Document as Document
-- import Graphql.Http.GraphqlError

import AniList.Enum.MediaFormat
import AniList.Object
import AniList.Object.Media as Media
import AniList.Object.MediaTitle as MediaTitle
import AniList.Object.Page as Page
import AniList.Query as Query
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Html.Attributes exposing (height, placeholder, src, style, value, width)
import Html.Events exposing (..)
import Http
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )
import Process exposing (Id)
import RemoteData exposing (RemoteData)



-- {
--   Page(page: 1, perPage: 20) {
--     media(type: MANGA) {
--       id
--       averageScore
--       title {
--         english
--       }
--     }
--   }
-- }
---- MODEL ----


type alias Response =
    Maybe Page


type alias Page =
    { media : Maybe (List (Maybe Media)) }


type alias Media =
    { id : Int, averageScore : Maybe Int, title : Maybe Title }


type alias Title =
    { english : Maybe String }


query : SelectionSet (Maybe Page) RootQuery
query =
    Query.page (\optionals -> { optionals | page = Present 1, perPage = Present 10 }) pageSelection



-- where `identity` replace with optional arguments to only dispaly 'manga'


pageSelection : SelectionSet Page AniList.Object.Page
pageSelection =
    SelectionSet.map Page (Page.media identity mediaSelection)


mediaSelection : SelectionSet Media AniList.Object.Media
mediaSelection =
    SelectionSet.map3 Media
        Media.id
        Media.averageScore
        (Media.title titleSelection)


titleSelection : SelectionSet Title AniList.Object.MediaTitle
titleSelection =
    SelectionSet.map Title
        (MediaTitle.english identity)


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


type Msg
    = GotResponse Model


type alias Model =
    RemoteData (Graphql.Http.Error Response) Response


init : () -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading, makeRequest )



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
    case model of
        RemoteData.Loading ->
            loadingSpinner

        RemoteData.NotAsked ->
            text "not asked is true"

        RemoteData.Failure _ ->
            text "unable to fetch genres"

        RemoteData.Success response ->
            case response of
                Just media ->
                    text (Debug.toString media)

                Nothing ->
                    text "No genres found."


loadingSpinner : Html Msg
loadingSpinner =
    div []
        [ Loading.render
            Circle
            { defaultConfig | color = "#333" }
            Loading.On
        ]


displayGenreList : List (Maybe String) -> Html Msg
displayGenreList genreList =
    div [ style "display" "flex", style "justify-content" "center" ]
        [ ul [ style "list-style-type" "none" ] (List.map displayGenre genreList)
        ]


displayGenre : Maybe String -> Html Msg
displayGenre genre =
    case genre of
        Just g ->
            li []
                [ h5 [] [ text g ]
                ]

        Nothing ->
            text "unknown genre"


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
