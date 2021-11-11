module Main exposing (..)

import AniList.Object
import AniList.Object.Media as Media
import AniList.Object.Page as Page
import AniList.Query as Query
import Browser
import Graphql.Document as Document
import Graphql.Http
import Graphql.Http.GraphqlError
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (..)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Html.Attributes exposing (height, placeholder, src, style, value, width)
import Html.Events exposing (..)
import Http
import Process exposing (Id)
import RemoteData exposing (RemoteData)



-- query {
--   Page(page: 1) {
--     media {
--       id
--     }
--   }
-- }
---- MODEL ----


type alias PageQuery =
    { page : MediaQuery }


type alias MediaQuery =
    { id : Int }


query : SelectionSet (Maybe PageQuery) RootQuery
query =
    Query.page (\optionals -> { optionals | page = Present 1, perPage = Present 20 }) pageSelection


pageSelection : SelectionSet PageQuery AniList.Object.Page
pageSelection =
    SelectionSet.map PageQuery mediaSelection


mediaSelection : SelectionSet MediaQuery AniList.Object.Media
mediaSelection =
    SelectionSet.map MediaQuery Media.id


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


type alias Model =
    RemoteData (Graphql.Http.Error (Maybe Int)) (Maybe Int)


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
                    text "Heh"

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
