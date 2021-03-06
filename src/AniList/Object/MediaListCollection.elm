-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.MediaListCollection exposing (..)

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


{-| Grouped media list entries
-}
lists :
    SelectionSet decodesTo AniList.Object.MediaListGroup
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.MediaListCollection
lists object____ =
    Object.selectionForCompositeField "lists" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| The owner of the list
-}
user :
    SelectionSet decodesTo AniList.Object.User
    -> SelectionSet (Maybe decodesTo) AniList.Object.MediaListCollection
user object____ =
    Object.selectionForCompositeField "user" [] object____ (Basics.identity >> Decode.nullable)


{-| If there is another chunk
-}
hasNextChunk : SelectionSet (Maybe Bool) AniList.Object.MediaListCollection
hasNextChunk =
    Object.selectionForField "(Maybe Bool)" "hasNextChunk" [] (Decode.bool |> Decode.nullable)


type alias StatusListsOptionalArguments =
    { asArray : OptionalArgument Bool }


{-| A map of media list entry arrays grouped by status
-}
statusLists :
    (StatusListsOptionalArguments -> StatusListsOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.MediaList
    -> SelectionSet (Maybe (List (Maybe (List (Maybe decodesTo))))) AniList.Object.MediaListCollection
statusLists fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { asArray = Absent }

        optionalArgs____ =
            [ Argument.optional "asArray" filledInOptionals____.asArray Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "statusLists" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias CustomListsOptionalArguments =
    { asArray : OptionalArgument Bool }


{-| A map of media list entry arrays grouped by custom lists
-}
customLists :
    (CustomListsOptionalArguments -> CustomListsOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.MediaList
    -> SelectionSet (Maybe (List (Maybe (List (Maybe decodesTo))))) AniList.Object.MediaListCollection
customLists fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { asArray = Absent }

        optionalArgs____ =
            [ Argument.optional "asArray" filledInOptionals____.asArray Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "customLists" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable >> Decode.list >> Decode.nullable)
