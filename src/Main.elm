module Main exposing (Msg(..))

-- import Home

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Home exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (href)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        )
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task exposing (perform)
import Url exposing (..)
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query



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


type alias Model =
    { key : Nav.Key, page : Page }


type Page
    = Home Home.Model
    | NotFound


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    stepUrl url { key = navKey, page = NotFound }


sendMsg : msg -> Cmd msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        HomeMsg msg ->
            case model.page of
                Home homeModel ->
                    stepHome Nothing model (Home.update msg homeModel)

                NotFound ->
                    ( model, Cmd.none )


route : Url.Parser.Parser a b -> a -> Url.Parser.Parser (b -> c) c
route parser handler =
    Url.Parser.map handler parser


stepHome : Maybe String -> Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome searchTerm model ( home, cmds ) =
    ( { model | page = Home { home | searchTerm = searchTerm } }, Cmd.map HomeMsg cmds )


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            Url.Parser.oneOf
                [ route (Url.Parser.top <?> Url.Parser.Query.string "search") (\queryStr -> stepHome queryStr model (Home.init queryStr model.key))
                ]
    in
    case Url.Parser.parse parser url of
        Just page ->
            page

        Nothing ->
            ( model, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Home homeModel ->
            Home.view HomeMsg homeModel

        NotFound ->
            { title = "elm-manga", body = [ div [] [ text "testing" ] ] }
