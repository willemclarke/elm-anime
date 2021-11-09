-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.ActivityLikeNotification exposing (..)

import AniList.Enum.NotificationType
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


{-| The id of the Notification
-}
id : SelectionSet Int AniList.Object.ActivityLikeNotification
id =
    Object.selectionForField "Int" "id" [] Decode.int


{-| The id of the user who liked to the activity
-}
userId : SelectionSet Int AniList.Object.ActivityLikeNotification
userId =
    Object.selectionForField "Int" "userId" [] Decode.int


{-| The type of notification
-}
type_ : SelectionSet (Maybe AniList.Enum.NotificationType.NotificationType) AniList.Object.ActivityLikeNotification
type_ =
    Object.selectionForField "(Maybe Enum.NotificationType.NotificationType)" "type" [] (AniList.Enum.NotificationType.decoder |> Decode.nullable)


{-| The id of the activity which was liked
-}
activityId : SelectionSet Int AniList.Object.ActivityLikeNotification
activityId =
    Object.selectionForField "Int" "activityId" [] Decode.int


{-| The notification context text
-}
context : SelectionSet (Maybe String) AniList.Object.ActivityLikeNotification
context =
    Object.selectionForField "(Maybe String)" "context" [] (Decode.string |> Decode.nullable)


{-| The time the notification was created at
-}
createdAt : SelectionSet (Maybe Int) AniList.Object.ActivityLikeNotification
createdAt =
    Object.selectionForField "(Maybe Int)" "createdAt" [] (Decode.int |> Decode.nullable)


{-| The liked activity
-}
activity :
    SelectionSet decodesTo AniList.Union.ActivityUnion
    -> SelectionSet (Maybe decodesTo) AniList.Object.ActivityLikeNotification
activity object____ =
    Object.selectionForCompositeField "activity" [] object____ (Basics.identity >> Decode.nullable)


{-| The user who liked the activity
-}
user :
    SelectionSet decodesTo AniList.Object.User
    -> SelectionSet (Maybe decodesTo) AniList.Object.ActivityLikeNotification
user object____ =
    Object.selectionForCompositeField "user" [] object____ (Basics.identity >> Decode.nullable)
