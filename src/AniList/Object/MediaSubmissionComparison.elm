-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.MediaSubmissionComparison exposing (..)

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


submission :
    SelectionSet decodesTo AniList.Object.MediaSubmissionEdge
    -> SelectionSet (Maybe decodesTo) AniList.Object.MediaSubmissionComparison
submission object____ =
    Object.selectionForCompositeField "submission" [] object____ (Basics.identity >> Decode.nullable)


character :
    SelectionSet decodesTo AniList.Object.MediaCharacter
    -> SelectionSet (Maybe decodesTo) AniList.Object.MediaSubmissionComparison
character object____ =
    Object.selectionForCompositeField "character" [] object____ (Basics.identity >> Decode.nullable)


staff :
    SelectionSet decodesTo AniList.Object.StaffEdge
    -> SelectionSet (Maybe decodesTo) AniList.Object.MediaSubmissionComparison
staff object____ =
    Object.selectionForCompositeField "staff" [] object____ (Basics.identity >> Decode.nullable)


studio :
    SelectionSet decodesTo AniList.Object.StudioEdge
    -> SelectionSet (Maybe decodesTo) AniList.Object.MediaSubmissionComparison
studio object____ =
    Object.selectionForCompositeField "studio" [] object____ (Basics.identity >> Decode.nullable)
