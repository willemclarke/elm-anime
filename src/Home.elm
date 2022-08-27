module Home exposing (..)

import Api
import Browser.Navigation
import Debounce exposing (Debounce)
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, href, id, name, placeholder, src, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        )
import Process
import RemoteData exposing (RemoteData(..))
import Route
import Task



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key
    , data : Api.MediaData
    , queryParams : Route.FilterQueryParams
    , debounce : Debounce String
    }


type alias SelectOption =
    { value : String, text : String }


init : Route.FilterQueryParams -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init params navKey =
    ( { key = navKey, data = RemoteData.Loading, queryParams = params, debounce = Debounce.init }, fetchManga params )


fetchManga : Route.FilterQueryParams -> Cmd Msg
fetchManga queryParams =
    Route.transformParams queryParams
        |> Api.query
        |> Graphql.Http.queryRequest "https://graphql.anilist.co/"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 500
    , transform = DebounceMsg
    }



-- UPDATE


type Msg
    = GotResponse Api.MediaData
    | ChangeInput String
    | DebounceMsg Debounce.Msg
    | SubmitInput String
    | ChangeMediaType String
    | ChangeGenre String
    | ChangeSortOption String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse resp ->
            ( { model | data = resp }, Cmd.none )

        ChangeInput newInput ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig newInput model.debounce
            in
            ( { model | debounce = debounce }
            , cmd
            )

        DebounceMsg msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast submitInput)
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }, cmd )

        SubmitInput string ->
            let
                searchTerm =
                    if String.isEmpty string then
                        Nothing

                    else
                        Just string

                newSearchParam =
                    updateSearchParam searchTerm model.queryParams
            in
            ( { model | data = RemoteData.Loading, queryParams = newSearchParam }, Route.addFilterParams model.key newSearchParam )

        ChangeMediaType mediaType ->
            let
                newMediaTypeParam =
                    updateMediaTypeParam mediaType model.queryParams
            in
            ( { model | data = RemoteData.Loading, queryParams = newMediaTypeParam }
            , Route.addFilterParams model.key newMediaTypeParam
            )

        ChangeGenre genre ->
            let
                newGenreParam =
                    updateGenreParam genre model.queryParams
            in
            ( { model | data = RemoteData.Loading, queryParams = newGenreParam }
            , Route.addFilterParams model.key newGenreParam
            )

        ChangeSortOption option ->
            let
                newSortParam =
                    updateSortParam option model.queryParams
            in
            ( { model | data = RemoteData.Loading, queryParams = newSortParam }
            , Route.addFilterParams model.key newSortParam
            )

        NoOp ->
            ( model, Cmd.none )


submitInput : String -> Cmd Msg
submitInput searchTerm =
    Task.perform SubmitInput (Task.succeed searchTerm)


updateSearchParam : Maybe String -> Route.FilterQueryParams -> Route.FilterQueryParams
updateSearchParam searchTerm params =
    { params | search = searchTerm }


updateMediaTypeParam : String -> Route.FilterQueryParams -> Route.FilterQueryParams
updateMediaTypeParam mediaType params =
    { params | mediaType = Just mediaType }


updateSortParam : String -> Route.FilterQueryParams -> Route.FilterQueryParams
updateSortParam option params =
    { params | sort = Just option }


updateGenreParam : String -> Route.FilterQueryParams -> Route.FilterQueryParams
updateGenreParam genre params =
    { params | genre = Just genre }



-- VIEW


view : Model -> Html Msg
view model =
    homeFrame model.queryParams.search model.queryParams.mediaType model.data


homeFrame : Maybe String -> Maybe String -> Api.MediaData -> Html Msg
homeFrame searchTerm mediaType mangaData =
    div [ class "flex flex-col h-full" ]
        [ filters searchTerm
        , displayMediaList mediaType mangaData
        ]



-- filter view functions


filters : Maybe String -> Html Msg
filters searchTerm =
    div [ onSubmit NoOp, class "mt-14 mx-16" ]
        [ form [ class "flex justify-center" ]
            [ searchFilter searchTerm
            , mediaTypeFiler
            , genreFilter
            , diverseSortFilter
            ]
        ]


searchFilter : Maybe String -> Html Msg
searchFilter searchTerm =
    div [ class "mr-9" ]
        [ div [ class "text-gray-700 font-bold" ] [ text "Search" ]
        , div []
            [ input [ class "mt-1 p-2 rounded shadow-l text-gray-700 h-10", placeholder "Search anime/manga", type_ "search", onInput ChangeInput ]
                [ text (Maybe.withDefault "" searchTerm) ]
            ]
        ]


mediaTypeFiler : Html Msg
mediaTypeFiler =
    let
        options =
            List.map (\mediaType -> option [ value mediaType.value ] [ text mediaType.text ]) mediaTypeOptions
    in
    div [ class "mr-9" ]
        [ div [ class "text-gray-700 font-bold" ] [ text "Type" ]
        , div []
            [ select [ name "genres", class "h-10 mt-1 p-2 rounded shadow-l text-gray-700 bg-white", onInput ChangeMediaType ]
                options
            ]
        ]


mediaTypeOptions : List SelectOption
mediaTypeOptions =
    [ { value = "ANIME", text = "Anime" }, { value = "MANGA", text = "Manga" } ]


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
    List.sort
        [ "Action"
        , "Adventure"
        , "Comedy"
        , "Sports"
        , "Drama"
        , "Psychological"
        , "Romance"
        , "Fantasy"
        , "Horror"
        , "Slice of Life"
        , "Sci-Fi"
        , "Mystery"
        , "Mecha"
        ]


diverseSortFilter : Html Msg
diverseSortFilter =
    let
        options =
            List.map (\diverseOption -> option [ value diverseOption.value ] [ text diverseOption.text ]) diverseSortOptions
    in
    div []
        [ div [ class "text-gray-700 font-bold" ] [ text "Sort" ]
        , div []
            [ select [ name "sort", class "h-10 mt-1 p-2 rounded shadow-l text-gray-700 bg-white", onInput ChangeSortOption ]
                options
            ]
        ]


diverseSortOptions : List SelectOption
diverseSortOptions =
    [ { value = "TRENDING_DESC", text = "Trending" }
    , { value = "POPULARITY_DESC", text = "Popular" }
    , { value = "SCORE_DESC", text = "Score Descending" }
    , { value = "FAVOURITES_DESC", text = "Most Favourited" }
    , { value = "START_DATE_DESC", text = "Latest" }
    , { value = "START_DATE", text = "Oldest" }
    ]



-- anime/manga view functions


displayMediaList : Maybe String -> Api.MediaData -> Html Msg
displayMediaList mediaType response =
    case response of
        Loading ->
            div [ class "flex h-full justify-center items-center" ]
                [ loadingSpinner
                ]

        NotAsked ->
            text "Not asked"

        Failure _ ->
            text "Failed to fetch list of anime/mangas"

        Success resp ->
            case resp of
                Just pageOfMedia ->
                    div [ class "flex justify-center" ]
                        [ div [ class "mx-16 mt-10 mb-16 grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-5 gap-x-10 gap-y-8" ]
                            (List.map (displayMedia mediaType) (Api.sanitizeMediaList pageOfMedia.media))
                        ]

                Nothing ->
                    text "No anime/manga to display"


displayMedia : Maybe String -> Api.Media -> Html Msg
displayMedia mediaType media =
    a [ href (mediaHref mediaType media.id) ]
        [ div [ class "w-48 h-90 text-center text-gray-700 bg-white rounded overflow-hidden shadow-lg hover:text-indigo-900 hover:shadow-2xl" ]
            [ img [ src (Api.sanitizeCoverImage media.coverImage), class "h-64 w-full" ]
                []
            , div
                []
                [ p [ class "text-l font-bold truncate mx-2 mt-1 hover:underline" ] [ text (Api.sanitizeTitle media.title) ]
                , displayGenres (Api.sanitizeGenres media.genres)
                ]
            ]
        ]


mediaHref : Maybe String -> Int -> String
mediaHref mediaType id =
    let
        stringMediaType =
            Maybe.withDefault "anime" mediaType

        word =
            String.toLower stringMediaType

        toStringId =
            String.fromInt id
    in
    if word == "anime" then
        "https://anilist.co/anime/" ++ toStringId

    else
        "https://anilist.co/manga/" ++ toStringId


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


loadingSpinner : Html Msg
loadingSpinner =
    div [ class "mb-96" ]
        [ Loading.render
            Circle
            { defaultConfig | color = "#1d4ed8", size = 40 }
            Loading.On
        ]
