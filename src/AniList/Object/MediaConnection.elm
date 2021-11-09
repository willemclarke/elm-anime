-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.MediaConnection exposing (..)

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


edges :
    SelectionSet decodesTo AniList.Object.MediaEdge
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.MediaConnection
edges object____ =
    Object.selectionForCompositeField "edges" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


nodes :
    SelectionSet decodesTo AniList.Object.Media
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.MediaConnection
nodes object____ =
    Object.selectionForCompositeField "nodes" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| The pagination information
-}
pageInfo :
    SelectionSet decodesTo AniList.Object.PageInfo
    -> SelectionSet (Maybe decodesTo) AniList.Object.MediaConnection
pageInfo object____ =
    Object.selectionForCompositeField "pageInfo" [] object____ (Basics.identity >> Decode.nullable)
