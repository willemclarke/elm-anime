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
    = Home (Maybe String) Home.Model


makeRequest : Maybe String -> Cmd Msg
makeRequest searchTerm =
    searchTerm
        |> query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url _ =
    changeRouteTo (Route.fromUrl url)



-- init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
-- init _ url key =
--     ( { data = RemoteData.Loading, key = key, url = url, route = Just (Route.fromUrl url), isLoading = True }, sendMsg (UrlChanged url) )


sendMsg : msg -> Cmd msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity



---- UPDATE ----


type Msg
    = GotResponse MangaData
    | ChangeInput String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | IsLoading
    | GotHomeMsg Home.Msg



-- changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
-- changeRouteTo route model =
--     -- Route.Home username ->
--     --     Home.init username
--     --         |> updateWith (Profile username) GotProfileMsg model
--     case route of
--         Route.Home searchTerm ->
--             ( model, Route.setQueryParam model.key (Maybe.withDefault "" searchTerm) )
--         Route.NotFound ->
--             ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IsLoading ->
            ( { model | isLoading = True }, Cmd.none )

        GotResponse response ->
            ( { model | data = response, isLoading = False }, Cmd.none )

        -- NOTE: once I have a way to debounce setting query param, I can set data = RemoteData.Loading
        ChangeInput newInput ->
            ( model, Route.setQueryParam model.key newInput )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        -- changeRouteTo function in here
        UrlChanged url ->
            let
                route =
                    Route.fromUrl url
            in
            case route of
                Route.Home searchTerm ->
                    ( { model | url = url, route = Just route }, makeRequest searchTerm )

                NotFound ->
                    ( { model | url = url, route = Just route }, Cmd.none )

        ( GotHomeMsg subMsg, Home searchTerm home ) ->
            Home.update subMsg home
                |> updateWith (Home searchTerm) GotHomeMsg model


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
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



-- In UrlChanged instead of binding route here, I need a changeRouteTo : Route -> Model (Model, Cmd Msg) which will handle what im
-- currently doing more smartly
---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Just (Route.Home searchTerm) ->
            baseLayout model.isLoading searchTerm model.data

        Just Route.NotFound ->
            { title = "elm-manga", body = [ div [] [ text "This page does not exist" ] ] }

        Nothing ->
            { title = "elm-manga", body = [ div [] [ text "This page does not exist" ] ] }



-- View functions


baseLayout : Bool -> Maybe String -> MangaData -> Browser.Document Msg
baseLayout isLoading searchTerm mangaData =
    { title = "elm-manga"
    , body =
        [ div [ class "flex justify-center h-full bg-gray-100 mt-6" ]
            [ if isLoading then
                div [] [ loadingSpinner ]

              else
                div [] [ siteTitle, filters searchTerm, displayMangaList mangaData ]
            ]
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


displayMangaList : MangaData -> Html Msg
displayMangaList response =
    case response of
        Loading ->
            loadingSpinner

        NotAsked ->
            text "Not asked"

        Failure _ ->
            text "Failed to fetch list of manga's"

        Success resp ->
            case resp of
                Just pageOfManga ->
                    div [ class "mx-16 mt-8 mb-16 grid grid-cols-1 sm:grid-cols-2 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-5 gap-6" ]
                        (List.map displayManga (sanitizeMangaList pageOfManga.manga))

                Nothing ->
                    text "No manga's to display"


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
