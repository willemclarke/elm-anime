-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.ThreadCommentMentionNotification exposing (..)

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
id : SelectionSet Int AniList.Object.ThreadCommentMentionNotification
id =
    Object.selectionForField "Int" "id" [] Decode.int


{-| The id of the user who mentioned the authenticated user
-}
userId : SelectionSet Int AniList.Object.ThreadCommentMentionNotification
userId =
    Object.selectionForField "Int" "userId" [] Decode.int


{-| The type of notification
-}
type_ : SelectionSet (Maybe AniList.Enum.NotificationType.NotificationType) AniList.Object.ThreadCommentMentionNotification
type_ =
    Object.selectionForField "(Maybe Enum.NotificationType.NotificationType)" "type" [] (AniList.Enum.NotificationType.decoder |> Decode.nullable)


{-| The id of the comment where mentioned
-}
commentId : SelectionSet Int AniList.Object.ThreadCommentMentionNotification
commentId =
    Object.selectionForField "Int" "commentId" [] Decode.int


{-| The notification context text
-}
context : SelectionSet (Maybe String) AniList.Object.ThreadCommentMentionNotification
context =
    Object.selectionForField "(Maybe String)" "context" [] (Decode.string |> Decode.nullable)


{-| The time the notification was created at
-}
createdAt : SelectionSet (Maybe Int) AniList.Object.ThreadCommentMentionNotification
createdAt =
    Object.selectionForField "(Maybe Int)" "createdAt" [] (Decode.int |> Decode.nullable)


{-| The thread that the relevant comment belongs to
-}
thread :
    SelectionSet decodesTo AniList.Object.Thread
    -> SelectionSet (Maybe decodesTo) AniList.Object.ThreadCommentMentionNotification
thread object____ =
    Object.selectionForCompositeField "thread" [] object____ (Basics.identity >> Decode.nullable)


{-| The thread comment that included the @ mention
-}
comment :
    SelectionSet decodesTo AniList.Object.ThreadComment
    -> SelectionSet (Maybe decodesTo) AniList.Object.ThreadCommentMentionNotification
comment object____ =
    Object.selectionForCompositeField "comment" [] object____ (Basics.identity >> Decode.nullable)


{-| The user who mentioned the authenticated user
-}
user :
    SelectionSet decodesTo AniList.Object.User
    -> SelectionSet (Maybe decodesTo) AniList.Object.ThreadCommentMentionNotification
user object____ =
    Object.selectionForCompositeField "user" [] object____ (Basics.identity >> Decode.nullable)
