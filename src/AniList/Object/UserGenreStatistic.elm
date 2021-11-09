-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.UserGenreStatistic exposing (..)

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


count : SelectionSet Int AniList.Object.UserGenreStatistic
count =
    Object.selectionForField "Int" "count" [] Decode.int


meanScore : SelectionSet Float AniList.Object.UserGenreStatistic
meanScore =
    Object.selectionForField "Float" "meanScore" [] Decode.float


minutesWatched : SelectionSet Int AniList.Object.UserGenreStatistic
minutesWatched =
    Object.selectionForField "Int" "minutesWatched" [] Decode.int


chaptersRead : SelectionSet Int AniList.Object.UserGenreStatistic
chaptersRead =
    Object.selectionForField "Int" "chaptersRead" [] Decode.int


mediaIds : SelectionSet (List (Maybe Int)) AniList.Object.UserGenreStatistic
mediaIds =
    Object.selectionForField "(List (Maybe Int))" "mediaIds" [] (Decode.int |> Decode.nullable |> Decode.list)


genre : SelectionSet (Maybe String) AniList.Object.UserGenreStatistic
genre =
    Object.selectionForField "(Maybe String)" "genre" [] (Decode.string |> Decode.nullable)
