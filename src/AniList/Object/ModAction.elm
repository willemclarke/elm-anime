-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.ModAction exposing (..)

import AniList.Enum.ModActionType
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


{-| The id of the action
-}
id : SelectionSet Int AniList.Object.ModAction
id =
    Object.selectionForField "Int" "id" [] Decode.int


user :
    SelectionSet decodesTo AniList.Object.User
    -> SelectionSet (Maybe decodesTo) AniList.Object.ModAction
user object____ =
    Object.selectionForCompositeField "user" [] object____ (Basics.identity >> Decode.nullable)


mod :
    SelectionSet decodesTo AniList.Object.User
    -> SelectionSet (Maybe decodesTo) AniList.Object.ModAction
mod object____ =
    Object.selectionForCompositeField "mod" [] object____ (Basics.identity >> Decode.nullable)


type_ : SelectionSet (Maybe AniList.Enum.ModActionType.ModActionType) AniList.Object.ModAction
type_ =
    Object.selectionForField "(Maybe Enum.ModActionType.ModActionType)" "type" [] (AniList.Enum.ModActionType.decoder |> Decode.nullable)


objectId : SelectionSet (Maybe Int) AniList.Object.ModAction
objectId =
    Object.selectionForField "(Maybe Int)" "objectId" [] (Decode.int |> Decode.nullable)


objectType : SelectionSet (Maybe String) AniList.Object.ModAction
objectType =
    Object.selectionForField "(Maybe String)" "objectType" [] (Decode.string |> Decode.nullable)


data : SelectionSet (Maybe String) AniList.Object.ModAction
data =
    Object.selectionForField "(Maybe String)" "data" [] (Decode.string |> Decode.nullable)


createdAt : SelectionSet Int AniList.Object.ModAction
createdAt =
    Object.selectionForField "Int" "createdAt" [] Decode.int
