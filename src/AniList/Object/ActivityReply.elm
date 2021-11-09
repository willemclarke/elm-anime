-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.ActivityReply exposing (..)

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


{-| The id of the reply
-}
id : SelectionSet Int AniList.Object.ActivityReply
id =
    Object.selectionForField "Int" "id" [] Decode.int


{-| The id of the replies creator
-}
userId : SelectionSet (Maybe Int) AniList.Object.ActivityReply
userId =
    Object.selectionForField "(Maybe Int)" "userId" [] (Decode.int |> Decode.nullable)


{-| The id of the parent activity
-}
activityId : SelectionSet (Maybe Int) AniList.Object.ActivityReply
activityId =
    Object.selectionForField "(Maybe Int)" "activityId" [] (Decode.int |> Decode.nullable)


type alias TextOptionalArguments =
    { asHtml : OptionalArgument Bool }


{-| The reply text

  - asHtml - Return the string in pre-parsed html instead of markdown

-}
text :
    (TextOptionalArguments -> TextOptionalArguments)
    -> SelectionSet (Maybe String) AniList.Object.ActivityReply
text fillInOptionals____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { asHtml = Absent }

        optionalArgs____ =
            [ Argument.optional "asHtml" filledInOptionals____.asHtml Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForField "(Maybe String)" "text" optionalArgs____ (Decode.string |> Decode.nullable)


{-| The amount of likes the reply has
-}
likeCount : SelectionSet Int AniList.Object.ActivityReply
likeCount =
    Object.selectionForField "Int" "likeCount" [] Decode.int


{-| If the currently authenticated user liked the reply
-}
isLiked : SelectionSet (Maybe Bool) AniList.Object.ActivityReply
isLiked =
    Object.selectionForField "(Maybe Bool)" "isLiked" [] (Decode.bool |> Decode.nullable)


{-| The time the reply was created at
-}
createdAt : SelectionSet Int AniList.Object.ActivityReply
createdAt =
    Object.selectionForField "Int" "createdAt" [] Decode.int


{-| The user who created reply
-}
user :
    SelectionSet decodesTo AniList.Object.User
    -> SelectionSet (Maybe decodesTo) AniList.Object.ActivityReply
user object____ =
    Object.selectionForCompositeField "user" [] object____ (Basics.identity >> Decode.nullable)


{-| The users who liked the reply
-}
likes :
    SelectionSet decodesTo AniList.Object.User
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.ActivityReply
likes object____ =
    Object.selectionForCompositeField "likes" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)
