module Home exposing (..)

import Api exposing (Manga, MangaData, sanitizeCoverImage, sanitizeGenres, sanitizeMangaList, sanitizeTitle)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, src, type_)
import Html.Events exposing (onInput)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        )
import RemoteData exposing (RemoteData(..))
import Route exposing (setQueryParam)


type alias Model =
    { data : MangaData, isLoading : Bool, key : Nav.Key }



-- Init
-- it might be wrong to have a Nav.Key in my model/init, since the main page will have this


init : () -> Nav.Key -> ( Model, Cmd Msg )
init _ key =
    ( { data = RemoteData.Loading, key = key, isLoading = True }, Cmd.none )



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "elm-manga", body = [] }



-- view functions


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



-- Update


type Msg
    = GotResponse MangaData
    | ChangeInput String
    | IsLoading


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IsLoading ->
            ( { model | isLoading = True }, Cmd.none )

        GotResponse resp ->
            ( { model | data = resp, isLoading = False }, Cmd.none )

        ChangeInput newInput ->
            ( model, setQueryParam model.key newInput )
