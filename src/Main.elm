module Main exposing (Msg(..))

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Home exposing (Model)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Loading
    exposing
        ( LoaderType(..)
        )
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Task exposing (perform)
import Url exposing (..)
import Url.Parser exposing ((</>), (<?>))



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
    = HomePage Home.Model
    | NotFound


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    updateUrl url { page = NotFound, key = navKey }



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotHomeMsg Home.Msg


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
            updateUrl url model

        GotHomeMsg homeMsg ->
            case model.page of
                HomePage homeModel ->
                    toHome model (Home.update homeMsg homeModel)

                NotFound ->
                    ( model, Cmd.none )


toHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
toHome model ( home, cmds ) =
    ( { model | page = HomePage home }, Cmd.map GotHomeMsg cmds )


updateUrl : Url.Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Route.fromUrl url of
        Just (Route.Home params) ->
            Home.init params model.key
                |> toHome model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.page of
                HomePage homeModel ->
                    Home.view homeModel
                        |> Html.map GotHomeMsg

                NotFound ->
                    pageNotFound
    in
    { title = "elm-manga"
    , body = [ pageFrame content ]
    }


pageFrame : Html Msg -> Html Msg
pageFrame content =
    div [ class "flex justify-center h-full bg-gray-100 mt-6" ]
        [ div [ class "w:xl-9/12" ]
            [ pageHeader
            , content
            ]
        ]


pageHeader : Html Msg
pageHeader =
    h1 [ class "text-center mt-9 text-3xl 2xl:text-4xl filter drop-shadow-sm font-extrabold text-transparent bg-clip-text bg-gradient-to-r from-blue-900 to-blue-400" ] [ text "elm-manga" ]


pageNotFound : Html Msg
pageNotFound =
    h1 [ class "mt-4 text-lg font-bold text-gray-700" ] [ text "Sorry, this page doesn't exist :(" ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
