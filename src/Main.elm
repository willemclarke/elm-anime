module Main exposing (..)

import AniList.Object
import AniList.Query as Query
import Browser
import Char exposing (toUpper)
import Graphql.Document as Document
import Graphql.Http
import Graphql.Http.GraphqlError
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Html.Attributes exposing (height, placeholder, src, style, value, width)
import Html.Events exposing (..)
import Http
import Maybe exposing (withDefault)
import RemoteData exposing (RemoteData)



---- MODEL ----


type alias AniListResponse =
    Maybe (List (Maybe String))


query : SelectionSet AniListResponse RootQuery
query =
    Query.genreCollection


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


type alias Model =
    RemoteData (Graphql.Http.Error AniListResponse) AniListResponse


init : () -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading, makeRequest )



---- UPDATE ----


type Msg
    = GotResponse Model


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
            text "Loading"

        RemoteData.NotAsked ->
            text "not asked is true"

        RemoteData.Failure _ ->
            text "unable to fetch genres"

        RemoteData.Success response ->
            case response of
                Just listOfGenres ->
                    displayGenreList listOfGenres

                Nothing ->
                    text "No genres found."


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



---- HELPERS ----
