-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Enum.MediaRankType exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The type of ranking

  - Rated - Ranking is based on the media's ratings/score
  - Popular - Ranking is based on the media's popularity

-}
type MediaRankType
    = Rated
    | Popular


list : List MediaRankType
list =
    [ Rated, Popular ]


decoder : Decoder MediaRankType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "RATED" ->
                        Decode.succeed Rated

                    "POPULAR" ->
                        Decode.succeed Popular

                    _ ->
                        Decode.fail ("Invalid MediaRankType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : MediaRankType -> String
toString enum____ =
    case enum____ of
        Rated ->
            "RATED"

        Popular ->
            "POPULAR"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe MediaRankType
fromString enumString____ =
    case enumString____ of
        "RATED" ->
            Just Rated

        "POPULAR" ->
            Just Popular

        _ ->
            Nothing
