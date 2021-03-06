-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Enum.MediaSource exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Source type the media was adapted from

  - Original - An original production not based of another work
  - Manga - Asian comic book
  - LightNovel - Written work published in volumes
  - VisualNovel - Video game driven primary by text and narrative
  - VideoGame - Video game
  - Other - Other
  - Novel - Version 2+ only. Written works not published in volumes
  - Doujinshi - Version 2+ only. Self-published works
  - Anime - Version 2+ only. Japanese Anime
  - WebNovel - Version 3 only. Written works published online
  - LiveAction - Version 3 only. Live action media such as movies or TV show
  - Game - Version 3 only. Games excluding video games
  - Comic - Version 3 only. Comics excluding manga
  - MultimediaProject - Version 3 only. Multimedia project
  - PictureBook - Version 3 only. Picture book

-}
type MediaSource
    = Original
    | Manga
    | LightNovel
    | VisualNovel
    | VideoGame
    | Other
    | Novel
    | Doujinshi
    | Anime
    | WebNovel
    | LiveAction
    | Game
    | Comic
    | MultimediaProject
    | PictureBook


list : List MediaSource
list =
    [ Original, Manga, LightNovel, VisualNovel, VideoGame, Other, Novel, Doujinshi, Anime, WebNovel, LiveAction, Game, Comic, MultimediaProject, PictureBook ]


decoder : Decoder MediaSource
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ORIGINAL" ->
                        Decode.succeed Original

                    "MANGA" ->
                        Decode.succeed Manga

                    "LIGHT_NOVEL" ->
                        Decode.succeed LightNovel

                    "VISUAL_NOVEL" ->
                        Decode.succeed VisualNovel

                    "VIDEO_GAME" ->
                        Decode.succeed VideoGame

                    "OTHER" ->
                        Decode.succeed Other

                    "NOVEL" ->
                        Decode.succeed Novel

                    "DOUJINSHI" ->
                        Decode.succeed Doujinshi

                    "ANIME" ->
                        Decode.succeed Anime

                    "WEB_NOVEL" ->
                        Decode.succeed WebNovel

                    "LIVE_ACTION" ->
                        Decode.succeed LiveAction

                    "GAME" ->
                        Decode.succeed Game

                    "COMIC" ->
                        Decode.succeed Comic

                    "MULTIMEDIA_PROJECT" ->
                        Decode.succeed MultimediaProject

                    "PICTURE_BOOK" ->
                        Decode.succeed PictureBook

                    _ ->
                        Decode.fail ("Invalid MediaSource type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : MediaSource -> String
toString enum____ =
    case enum____ of
        Original ->
            "ORIGINAL"

        Manga ->
            "MANGA"

        LightNovel ->
            "LIGHT_NOVEL"

        VisualNovel ->
            "VISUAL_NOVEL"

        VideoGame ->
            "VIDEO_GAME"

        Other ->
            "OTHER"

        Novel ->
            "NOVEL"

        Doujinshi ->
            "DOUJINSHI"

        Anime ->
            "ANIME"

        WebNovel ->
            "WEB_NOVEL"

        LiveAction ->
            "LIVE_ACTION"

        Game ->
            "GAME"

        Comic ->
            "COMIC"

        MultimediaProject ->
            "MULTIMEDIA_PROJECT"

        PictureBook ->
            "PICTURE_BOOK"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe MediaSource
fromString enumString____ =
    case enumString____ of
        "ORIGINAL" ->
            Just Original

        "MANGA" ->
            Just Manga

        "LIGHT_NOVEL" ->
            Just LightNovel

        "VISUAL_NOVEL" ->
            Just VisualNovel

        "VIDEO_GAME" ->
            Just VideoGame

        "OTHER" ->
            Just Other

        "NOVEL" ->
            Just Novel

        "DOUJINSHI" ->
            Just Doujinshi

        "ANIME" ->
            Just Anime

        "WEB_NOVEL" ->
            Just WebNovel

        "LIVE_ACTION" ->
            Just LiveAction

        "GAME" ->
            Just Game

        "COMIC" ->
            Just Comic

        "MULTIMEDIA_PROJECT" ->
            Just MultimediaProject

        "PICTURE_BOOK" ->
            Just PictureBook

        _ ->
            Nothing
