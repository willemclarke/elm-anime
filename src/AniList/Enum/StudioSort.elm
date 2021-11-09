-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Enum.StudioSort exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Studio sort enums
-}
type StudioSort
    = Id
    | IdDesc
    | Name
    | NameDesc
    | SearchMatch
    | Favourites
    | FavouritesDesc


list : List StudioSort
list =
    [ Id, IdDesc, Name, NameDesc, SearchMatch, Favourites, FavouritesDesc ]


decoder : Decoder StudioSort
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ID" ->
                        Decode.succeed Id

                    "ID_DESC" ->
                        Decode.succeed IdDesc

                    "NAME" ->
                        Decode.succeed Name

                    "NAME_DESC" ->
                        Decode.succeed NameDesc

                    "SEARCH_MATCH" ->
                        Decode.succeed SearchMatch

                    "FAVOURITES" ->
                        Decode.succeed Favourites

                    "FAVOURITES_DESC" ->
                        Decode.succeed FavouritesDesc

                    _ ->
                        Decode.fail ("Invalid StudioSort type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : StudioSort -> String
toString enum____ =
    case enum____ of
        Id ->
            "ID"

        IdDesc ->
            "ID_DESC"

        Name ->
            "NAME"

        NameDesc ->
            "NAME_DESC"

        SearchMatch ->
            "SEARCH_MATCH"

        Favourites ->
            "FAVOURITES"

        FavouritesDesc ->
            "FAVOURITES_DESC"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe StudioSort
fromString enumString____ =
    case enumString____ of
        "ID" ->
            Just Id

        "ID_DESC" ->
            Just IdDesc

        "NAME" ->
            Just Name

        "NAME_DESC" ->
            Just NameDesc

        "SEARCH_MATCH" ->
            Just SearchMatch

        "FAVOURITES" ->
            Just Favourites

        "FAVOURITES_DESC" ->
            Just FavouritesDesc

        _ ->
            Nothing
