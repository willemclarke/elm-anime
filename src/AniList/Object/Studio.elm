-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.Studio exposing (..)

import AniList.Enum.MediaSort
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


{-| The id of the studio
-}
id : SelectionSet Int AniList.Object.Studio
id =
    Object.selectionForField "Int" "id" [] Decode.int


{-| The name of the studio
-}
name : SelectionSet String AniList.Object.Studio
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| If the studio is an animation studio or a different kind of company
-}
isAnimationStudio : SelectionSet Bool AniList.Object.Studio
isAnimationStudio =
    Object.selectionForField "Bool" "isAnimationStudio" [] Decode.bool


type alias MediaOptionalArguments =
    { sort : OptionalArgument (List (Maybe AniList.Enum.MediaSort.MediaSort))
    , isMain : OptionalArgument Bool
    , onList : OptionalArgument Bool
    , page : OptionalArgument Int
    , perPage : OptionalArgument Int
    }


{-| The media the studio has worked on

  - sort - The order the results will be returned in
  - isMain - If the studio was the primary animation studio of the media
  - page - The page
  - perPage - The amount of entries per page, max 25

-}
media :
    (MediaOptionalArguments -> MediaOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.MediaConnection
    -> SelectionSet (Maybe decodesTo) AniList.Object.Studio
media fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { sort = Absent, isMain = Absent, onList = Absent, page = Absent, perPage = Absent }

        optionalArgs____ =
            [ Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.MediaSort.toString |> Encode.maybe |> Encode.list), Argument.optional "isMain" filledInOptionals____.isMain Encode.bool, Argument.optional "onList" filledInOptionals____.onList Encode.bool, Argument.optional "page" filledInOptionals____.page Encode.int, Argument.optional "perPage" filledInOptionals____.perPage Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "media" optionalArgs____ object____ (Basics.identity >> Decode.nullable)


{-| The url for the studio page on the AniList website
-}
siteUrl : SelectionSet (Maybe String) AniList.Object.Studio
siteUrl =
    Object.selectionForField "(Maybe String)" "siteUrl" [] (Decode.string |> Decode.nullable)


{-| If the studio is marked as favourite by the currently authenticated user
-}
isFavourite : SelectionSet Bool AniList.Object.Studio
isFavourite =
    Object.selectionForField "Bool" "isFavourite" [] Decode.bool


{-| The amount of user's who have favourited the studio
-}
favourites : SelectionSet (Maybe Int) AniList.Object.Studio
favourites =
    Object.selectionForField "(Maybe Int)" "favourites" [] (Decode.int |> Decode.nullable)
