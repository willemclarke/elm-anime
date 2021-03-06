-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Enum.ActivityType exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Activity type enum.

  - Text - A text activity
  - AnimeList - A anime list update activity
  - MangaList - A manga list update activity
  - Message - A text message activity sent to another user
  - MediaList - Anime & Manga list update, only used in query arguments

-}
type ActivityType
    = Text
    | AnimeList
    | MangaList
    | Message
    | MediaList


list : List ActivityType
list =
    [ Text, AnimeList, MangaList, Message, MediaList ]


decoder : Decoder ActivityType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "TEXT" ->
                        Decode.succeed Text

                    "ANIME_LIST" ->
                        Decode.succeed AnimeList

                    "MANGA_LIST" ->
                        Decode.succeed MangaList

                    "MESSAGE" ->
                        Decode.succeed Message

                    "MEDIA_LIST" ->
                        Decode.succeed MediaList

                    _ ->
                        Decode.fail ("Invalid ActivityType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ActivityType -> String
toString enum____ =
    case enum____ of
        Text ->
            "TEXT"

        AnimeList ->
            "ANIME_LIST"

        MangaList ->
            "MANGA_LIST"

        Message ->
            "MESSAGE"

        MediaList ->
            "MEDIA_LIST"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ActivityType
fromString enumString____ =
    case enumString____ of
        "TEXT" ->
            Just Text

        "ANIME_LIST" ->
            Just AnimeList

        "MANGA_LIST" ->
            Just MangaList

        "MESSAGE" ->
            Just Message

        "MEDIA_LIST" ->
            Just MediaList

        _ ->
            Nothing
