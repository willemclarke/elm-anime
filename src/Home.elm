module Home exposing (..)

import Api
import Browser.Navigation
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, href, name, placeholder, selected, src, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        )
import Process
import RemoteData exposing (RemoteData(..))
import Route



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key, data : Api.MangaData, searchTerm : Maybe String, genre : Maybe String, sort : Maybe String }


init : Route.FilterQueryParams -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init params navKey =
    ( { key = navKey, data = RemoteData.Loading, searchTerm = params.search, genre = params.genre, sort = params.sort }, fetchManga params )


fetchManga : Route.FilterQueryParams -> Cmd Msg
fetchManga queryParams =
    Route.transformParams queryParams
        |> Api.query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



-- UPDATE


type Msg
    = GotResponse Api.MangaData
    | ChangeInput String
    | ChangeGenre String
    | ChangeSortOption String
    | OnSubmit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse resp ->
            ( { model | data = resp }, Cmd.none )

        ChangeInput newInput ->
            let
                searchTerm =
                    if String.isEmpty newInput then
                        Nothing

                    else
                        Just newInput
            in
            ( { model | searchTerm = searchTerm }, Cmd.none )

        OnSubmit ->
            ( { model | data = RemoteData.Loading }
            , Route.addFilterParams model.key
                { search = model.searchTerm, genre = model.genre, sort = model.sort }
            )

        ChangeGenre genre ->
            ( { model | data = RemoteData.Loading, genre = Just genre }
            , Route.addFilterParams model.key
                { genre = Just genre, search = model.searchTerm, sort = model.sort }
            )

        ChangeSortOption option ->
            ( { model | data = RemoteData.Loading, sort = Just option }
            , Route.addFilterParams model.key
                { sort = Just option, search = model.searchTerm, genre = model.genre }
            )



-- VIEW


view : Model -> Html Msg
view model =
    homeFrame model.searchTerm model.data


homeFrame : Maybe String -> Api.MangaData -> Html Msg
homeFrame searchTerm mangaData =
    div []
        [ filters searchTerm
        , displayMangaList mangaData
        ]


filters : Maybe String -> Html Msg
filters searchTerm =
    div [ class "mt-10 mx-16" ]
        [ form [ onSubmit OnSubmit, class "flex" ]
            [ searchFilter searchTerm
            , genreFilter
            , diverseSortFilter
            ]
        ]


searchFilter : Maybe String -> Html Msg
searchFilter searchTerm =
    div [ class "mr-9" ]
        [ div [ class "text-gray-700 font-bold" ] [ text "Search" ]
        , div []
            [ input [ class "mt-1 p-2 rounded shadow-l text-gray-700 h-10", placeholder "Search manga", type_ "search", onInput ChangeInput ]
                [ text (Maybe.withDefault "" searchTerm) ]
            ]
        ]


genreFilter : Html Msg
genreFilter =
    let
        options =
            List.map (\genre -> option [ value genre ] [ text genre ]) listOfGenres
    in
    div [ class "mr-9" ]
        [ div [ class "text-gray-700 font-bold" ] [ text "Genres" ]
        , div []
            [ select [ name "genres", class "h-10 mt-1 p-2 rounded shadow-l text-gray-700 bg-white", onInput ChangeGenre ]
                options
            ]
        ]


listOfGenres : List String
listOfGenres =
    List.sort [ "Action", "Adventure", "Comedy", "Drama", "Psychological", "Romance", "Fantasy", "Horror", "Slice of Life", "Sci-Fi", "Mystery", "Mecha" ]


diverseSortFilter : Html Msg
diverseSortFilter =
    div []
        [ div [ class "text-gray-700 font-bold" ] [ text "Sort" ]
        , div []
            [ select [ name "sort", class "h-10 mt-1 p-2 rounded shadow-l text-gray-700 bg-white", onInput ChangeSortOption ]
                [ option
                    [ value "POPULARITY_DESC" ]
                    [ text "Most Popular" ]
                , option [ value "TRENDING_DESC" ] [ text "Trending Now" ]
                ]
            ]
        ]


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
                    div [ class "mx-16 mt-8 mb-16 grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 gap-6" ]
                        (List.map displayManga (Api.sanitizeMangaList pageOfManga.manga))

                Nothing ->
                    text "No manga's to display"


displayManga : Api.Manga -> Html Msg
displayManga manga =
    a [ href ("https://anilist.co/manga/" ++ String.fromInt manga.id) ]
        [ div [ class "w-48 h-90 text-center text-gray-700 bg-white rounded overflow-hidden shadow-lg hover:text-indigo-900 hover:shadow-2xl" ]
            [ img [ src (Api.sanitizeCoverImage manga.coverImage), class "h-64 w-full" ]
                []
            , div
                []
                [ p [ class "text-l font-bold truncate mx-2 mt-1 hover:underline" ] [ text (Api.sanitizeTitle manga.title) ]
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
        div [ class "my-1.5" ]
            [ span [ class "mx-2 text-md font-semibold text-gray-700" ] [ text "No genres" ]
            ]

    else
        div [ class "mx-2 mt-1 mb-2" ]
            (List.map (\genre -> span [ class "inline-block bg-blue-300 rounded-full px-2 text-xs font-semibold text-gray-700 mr-2" ] [ text genre ]) firstTwoGenres)
