-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Enum.StaffLanguage exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The primary language of the voice actor

  - Japanese - Japanese
  - English - English
  - Korean - Korean
  - Italian - Italian
  - Spanish - Spanish
  - Portuguese - Portuguese
  - French - French
  - German - German
  - Hebrew - Hebrew
  - Hungarian - Hungarian

-}
type StaffLanguage
    = Japanese
    | English
    | Korean
    | Italian
    | Spanish
    | Portuguese
    | French
    | German
    | Hebrew
    | Hungarian


list : List StaffLanguage
list =
    [ Japanese, English, Korean, Italian, Spanish, Portuguese, French, German, Hebrew, Hungarian ]


decoder : Decoder StaffLanguage
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "JAPANESE" ->
                        Decode.succeed Japanese

                    "ENGLISH" ->
                        Decode.succeed English

                    "KOREAN" ->
                        Decode.succeed Korean

                    "ITALIAN" ->
                        Decode.succeed Italian

                    "SPANISH" ->
                        Decode.succeed Spanish

                    "PORTUGUESE" ->
                        Decode.succeed Portuguese

                    "FRENCH" ->
                        Decode.succeed French

                    "GERMAN" ->
                        Decode.succeed German

                    "HEBREW" ->
                        Decode.succeed Hebrew

                    "HUNGARIAN" ->
                        Decode.succeed Hungarian

                    _ ->
                        Decode.fail ("Invalid StaffLanguage type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : StaffLanguage -> String
toString enum____ =
    case enum____ of
        Japanese ->
            "JAPANESE"

        English ->
            "ENGLISH"

        Korean ->
            "KOREAN"

        Italian ->
            "ITALIAN"

        Spanish ->
            "SPANISH"

        Portuguese ->
            "PORTUGUESE"

        French ->
            "FRENCH"

        German ->
            "GERMAN"

        Hebrew ->
            "HEBREW"

        Hungarian ->
            "HUNGARIAN"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe StaffLanguage
fromString enumString____ =
    case enumString____ of
        "JAPANESE" ->
            Just Japanese

        "ENGLISH" ->
            Just English

        "KOREAN" ->
            Just Korean

        "ITALIAN" ->
            Just Italian

        "SPANISH" ->
            Just Spanish

        "PORTUGUESE" ->
            Just Portuguese

        "FRENCH" ->
            Just French

        "GERMAN" ->
            Just German

        "HEBREW" ->
            Just Hebrew

        "HUNGARIAN" ->
            Just Hungarian

        _ ->
            Nothing
