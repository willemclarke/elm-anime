-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.FuzzyDate exposing (..)

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


{-| Numeric Year (2017)
-}
year : SelectionSet (Maybe Int) AniList.Object.FuzzyDate
year =
    Object.selectionForField "(Maybe Int)" "year" [] (Decode.int |> Decode.nullable)


{-| Numeric Month (3)
-}
month : SelectionSet (Maybe Int) AniList.Object.FuzzyDate
month =
    Object.selectionForField "(Maybe Int)" "month" [] (Decode.int |> Decode.nullable)


{-| Numeric Day (24)
-}
day : SelectionSet (Maybe Int) AniList.Object.FuzzyDate
day =
    Object.selectionForField "(Maybe Int)" "day" [] (Decode.int |> Decode.nullable)
