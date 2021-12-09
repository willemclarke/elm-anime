module Main exposing (Msg(..))

-- import Home

import Api exposing (Manga, MangaData, query, sanitizeCoverImage, sanitizeGenres, sanitizeMangaList, sanitizeTitle)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Graphql.Http
import Home
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, src, type_)
import Html.Events exposing (onInput)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        )
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task exposing (perform)
import Url exposing (..)



--- MAIN ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL ----
-- type alias Model =
--     { data : MangaData, key : Nav.Key, url : Url.Url, route : Maybe Route, isLoading : Bool }


type Model
    = Blank
    | Home (Maybe String) Home.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo (Route.fromUrl url) Blank



-- init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
-- init _ url key =
--     ( { data = RemoteData.Loading, key = key, url = url, route = Just (Route.fromUrl url), isLoading = True }, sendMsg (UrlChanged url) )


sendMsg : msg -> Cmd msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home searchTerm home ) ->
            Home.update subMsg home
                |> updateWith (Home searchTerm) GotHomeMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Nothing ->
            ( model, Cmd.none )

        Just Route.NotFound ->
            ( model, Cmd.none )

        Just (Route.Home searchTerm) ->
            Home.init searchTerm
                |> updateWith (Home searchTerm) GotHomeMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model of
        Blank ->
            { title = "elm-manga", body = [ div [] [ text "This page does not exist" ] ] }

        Home searchTerm homeModel ->
            Home.view (searchTerm homeModel) GotHomeMsg
