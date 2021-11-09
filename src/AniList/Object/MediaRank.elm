-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.MediaRank exposing (..)

import AniList.Enum.MediaFormat
import AniList.Enum.MediaRankType
import AniList.Enum.MediaSeason
import AniList.InputObject
import AniList.Interface
import AniList.Object
import AniList.Scalar
import AniList.ScalarCodecs
import AniList.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| The id of the rank
-}
id : SelectionSet Int AniList.Object.MediaRank
id =
    Object.selectionForField "Int" "id" [] Decode.int


{-| The numerical rank of the media
-}
rank : SelectionSet Int AniList.Object.MediaRank
rank =
    Object.selectionForField "Int" "rank" [] Decode.int


{-| The type of ranking
-}
type_ : SelectionSet AniList.Enum.MediaRankType.MediaRankType AniList.Object.MediaRank
type_ =
    Object.selectionForField "Enum.MediaRankType.MediaRankType" "type" [] AniList.Enum.MediaRankType.decoder


{-| The format the media is ranked within
-}
format : SelectionSet AniList.Enum.MediaFormat.MediaFormat AniList.Object.MediaRank
format =
    Object.selectionForField "Enum.MediaFormat.MediaFormat" "format" [] AniList.Enum.MediaFormat.decoder


{-| The year the media is ranked within
-}
year : SelectionSet (Maybe Int) AniList.Object.MediaRank
year =
    Object.selectionForField "(Maybe Int)" "year" [] (Decode.int |> Decode.nullable)


{-| The season the media is ranked within
-}
season : SelectionSet (Maybe AniList.Enum.MediaSeason.MediaSeason) AniList.Object.MediaRank
season =
    Object.selectionForField "(Maybe Enum.MediaSeason.MediaSeason)" "season" [] (AniList.Enum.MediaSeason.decoder |> Decode.nullable)


{-| If the ranking is based on all time instead of a season/year
-}
allTime : SelectionSet (Maybe Bool) AniList.Object.MediaRank
allTime =
    Object.selectionForField "(Maybe Bool)" "allTime" [] (Decode.bool |> Decode.nullable)


{-| String that gives context to the ranking type and time span
-}
context : SelectionSet String AniList.Object.MediaRank
context =
    Object.selectionForField "String" "context" [] Decode.string
