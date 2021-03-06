-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Enum.MediaType exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Media type enum, anime or manga.

  - Anime - Japanese Anime
  - Manga - Asian comic

-}
type MediaType
    = Anime
    | Manga


list : List MediaType
list =
    [ Anime, Manga ]


decoder : Decoder MediaType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ANIME" ->
                        Decode.succeed Anime

                    "MANGA" ->
                        Decode.succeed Manga

                    _ ->
                        Decode.fail ("Invalid MediaType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : MediaType -> String
toString enum____ =
    case enum____ of
        Anime ->
            "ANIME"

        Manga ->
            "MANGA"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe MediaType
fromString enumString____ =
    case enumString____ of
        "ANIME" ->
            Just Anime

        "MANGA" ->
            Just Manga

        _ ->
            Nothing
