module Main exposing (Msg(..))

import API.Api exposing (Manga, MangaData, query, sanitizeCoverImage, sanitizeGenres, sanitizeMangaList, sanitizeTitle)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, src, type_)
import Html.Events exposing (onInput)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        )
import RemoteData
import Route exposing (Route(..), fromUrl, setQueryParam)
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



---- MODEL ----


type alias Model =
    { data : MangaData, key : Nav.Key, url : Url.Url, route : Maybe Route }


makeRequest : Maybe String -> Cmd Msg
makeRequest searchTerm =
    searchTerm
        |> query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { data = RemoteData.Loading, key = key, url = url, route = Just (fromUrl url) }, send (UrlChanged url) )


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity



---- UPDATE ----


type Msg
    = GotResponse MangaData
    | ChangeInput String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( { model | data = response }, Cmd.none )

        ChangeInput newInput ->
            ( model, setQueryParam model.key newInput )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    fromUrl url
            in
            case route of
                Home searchTerm ->
                    ( { model | url = url, route = Just route }, makeRequest searchTerm )

                NotFound ->
                    ( { model | url = url, route = Just route }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        renderHomePage searchTerm =
            case model.data of
                RemoteData.Loading ->
                    loadingSpinner

                RemoteData.NotAsked ->
                    text "Not asked"

                RemoteData.Failure _ ->
                    text "Unable to fetch mangas"

                RemoteData.Success response ->
                    case response of
                        Just page ->
                            div []
                                [ siteTitle
                                , filters searchTerm
                                , p [] [ text (Debug.toString (fromUrl model.url)) ]
                                , p [] [ text (Debug.toString model.route) ]
                                , displayMangaList
                                    (sanitizeMangaList page.manga)
                                ]

                        Nothing ->
                            text "No manga's found."
    in
    case model.route of
        Just (Home searchTerm) ->
            baseLayout (renderHomePage searchTerm)

        Just NotFound ->
            { title = "elm-layout", body = [ div [] [ text "Invalid route" ] ] }

        Nothing ->
            { title = "elm-layout", body = [ div [] [ text "Invalid route" ] ] }



-- View functions


baseLayout : Html Msg -> Browser.Document Msg
baseLayout children =
    { title = "elm-manga"
    , body =
        [ div [ class "flex justify-center h-full bg-gray-100 mt-6" ]
            [ children ]
        ]
    }


siteTitle : Html Msg
siteTitle =
    h1 [ class "text-center mt-2 text-3xl 2xl:text-4xl filter drop-shadow-sm font-extrabold text-transparent bg-clip-text bg-gradient-to-r from-blue-900 to-blue-400" ] [ text "elm-manga" ]


filters : Maybe String -> Html Msg
filters searchTerm =
    div [ class "flex justify-start mt-10 mx-16" ] [ searchFilter searchTerm ]


searchFilter : Maybe String -> Html Msg
searchFilter searchTerm =
    div []
        [ div [ class "text-gray-700 font-bold" ] [ text "Search" ]
        , div []
            [ form []
                [ input [ class "mt-1 p-2 rounded shadow-l text-gray-700 ", placeholder "Search manga", type_ "search", onInput ChangeInput ] [ text (Maybe.withDefault "" searchTerm) ]
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
    if List.length firstTwoGenres /= 2 then
        span [ class "px-2 text-md font-semibold text-gray-700 mr-1 mb-1" ] [ text "No genres" ]

    else
        div [ class "mx-2" ]
            (List.map (\genre -> span [ class "inline-block bg-blue-200 rounded-full px-2 text-xs font-semibold text-gray-700 mr-1 mb-1" ] [ text genre ]) firstTwoGenres)
