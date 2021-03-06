-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.PageInfo exposing (..)

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


{-| The total number of items
-}
total : SelectionSet (Maybe Int) AniList.Object.PageInfo
total =
    Object.selectionForField "(Maybe Int)" "total" [] (Decode.int |> Decode.nullable)


{-| The count on a page
-}
perPage : SelectionSet (Maybe Int) AniList.Object.PageInfo
perPage =
    Object.selectionForField "(Maybe Int)" "perPage" [] (Decode.int |> Decode.nullable)


{-| The current page
-}
currentPage : SelectionSet (Maybe Int) AniList.Object.PageInfo
currentPage =
    Object.selectionForField "(Maybe Int)" "currentPage" [] (Decode.int |> Decode.nullable)


{-| The last page
-}
lastPage : SelectionSet (Maybe Int) AniList.Object.PageInfo
lastPage =
    Object.selectionForField "(Maybe Int)" "lastPage" [] (Decode.int |> Decode.nullable)


{-| If there is another page
-}
hasNextPage : SelectionSet (Maybe Bool) AniList.Object.PageInfo
hasNextPage =
    Object.selectionForField "(Maybe Bool)" "hasNextPage" [] (Decode.bool |> Decode.nullable)
