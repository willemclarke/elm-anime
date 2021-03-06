-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.StaffRoleType exposing (..)

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


{-| The voice actors of the character
-}
voiceActor :
    SelectionSet decodesTo AniList.Object.Staff
    -> SelectionSet (Maybe decodesTo) AniList.Object.StaffRoleType
voiceActor object____ =
    Object.selectionForCompositeField "voiceActor" [] object____ (Basics.identity >> Decode.nullable)


{-| Notes regarding the VA's role for the character
-}
roleNotes : SelectionSet (Maybe String) AniList.Object.StaffRoleType
roleNotes =
    Object.selectionForField "(Maybe String)" "roleNotes" [] (Decode.string |> Decode.nullable)


{-| Used for grouping roles where multiple dubs exist for the same language. Either dubbing company name or language variant.
-}
dubGroup : SelectionSet (Maybe String) AniList.Object.StaffRoleType
dubGroup =
    Object.selectionForField "(Maybe String)" "dubGroup" [] (Decode.string |> Decode.nullable)
