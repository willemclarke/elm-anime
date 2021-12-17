module Home exposing (..)

import Api
import Browser.Navigation
import Graphql.Http
import Graphql.OptionalArgument exposing (fromMaybe)
import Html exposing (..)
import Html.Attributes exposing (class, href, name, placeholder, src, type_, value)
import Html.Events exposing (on, onInput)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        )
import RemoteData exposing (RemoteData(..))
import Route



-- MODEL


type alias Model =
    { data : Api.MangaData, searchTerm : Maybe String, key : Browser.Navigation.Key }


init : Route.FilterQueryParams -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init queryParams navKey =
    ( { data = RemoteData.Loading, searchTerm = Nothing, key = navKey }, makeRequest queryParams )


makeRequest : Route.FilterQueryParams -> Cmd Msg
makeRequest queryParams =
    transformParams queryParams
        |> Api.query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



-- function to convert /?query="x"&genre="y" to Present/Absent of x/y


transformParams : Route.FilterQueryParams -> Api.Filter
transformParams filterQueryPrams =
    { search = fromMaybe filterQueryPrams.search, genre = fromMaybe filterQueryPrams.genre }



-- UPDATE


type Msg
    = GotResponse Api.MangaData
    | ChangeInput String
    | ChangeGenre String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse resp ->
            ( { model | data = resp }, Cmd.none )

        ChangeInput newInput ->
            ( { model | data = RemoteData.Loading, searchTerm = Just newInput }, Route.setSearchParam model.key newInput )

        ChangeGenre genre ->
            ( { model | data = RemoteData.Loading }, Route.setGenreParam model.key genre )



-- VIEW


view : Model -> Html Msg
view model =
    homeFrame model.searchTerm model.data


homeFrame : Maybe String -> Api.MangaData -> Html Msg
homeFrame searchTerm mangaData =
    div [] [ filters searchTerm, displayMangaList mangaData ]


filters : Maybe String -> Html Msg
filters searchTerm =
    div [ class "flex mt-10 mx-16 " ]
        [ form [ class "flex" ]
            [ searchFilter searchTerm
            , genreFilter
            ]
        ]


searchFilter : Maybe String -> Html Msg
searchFilter searchTerm =
    div [ class "mr-9" ]
        [ div [ class "text-gray-700 font-bold" ] [ text "Search" ]
        , div []
            [ input [ class "mt-1 p-2 rounded shadow-l text-gray-700 ", placeholder "Search manga", type_ "search", onInput ChangeInput ]
                [ text (Maybe.withDefault "" searchTerm) ]
            ]
        ]


genreFilter : Html Msg
genreFilter =
    let
        options =
            List.map (\genre -> option [ value genre ] [ text genre ]) listOfGenres
    in
    div []
        [ div [ class "text-gray-700 font-bold" ] [ text "Genres" ]
        , div []
            [ select [ name "genres", class "mt-1 p-2 rounded shadow-l text-gray-700 bg-white", onInput ChangeGenre ]
                options
            ]
        ]


listOfGenres : List String
listOfGenres =
    [ "Action", "Adventure", "Comedy", "Drama", "Psychological", "Romance", "Fantasy", "Horror" ]


loadingSpinner : Html Msg
loadingSpinner =
    div [ class "flex h-full justify-center items-center mt-8" ]
        [ Loading.render
            Circle
            { defaultConfig | color = "#333" }
            Loading.On
        ]


displayMangaList : Api.MangaData -> Html Msg
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
                        (List.map displayManga (Api.sanitizeMangaList pageOfManga.manga))

                Nothing ->
                    text "No manga's to display"


displayManga : Api.Manga -> Html Msg
displayManga manga =
    a [ href ("https://anilist.co/manga/" ++ String.fromInt manga.id) ]
        [ div [ class "w-48 h-80 text-center text-gray-700 bg-white rounded overflow-hidden shadow-lg hover:text-indigo-900 hover:shadow-2xl" ]
            [ img [ src (Api.sanitizeCoverImage manga.coverImage), class "h-64 w-full" ]
                []
            , div
                []
                [ p [ class "text-l font-bold hover:font-black truncate mx-2 mt-1 mb-1" ] [ text (Api.sanitizeTitle manga.title) ]
                , displayGenres (Api.sanitizeGenres manga.genres)
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
