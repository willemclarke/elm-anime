-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module AniList.Object.UserStatistics exposing (..)

import AniList.Enum.UserStatisticsSort
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


count : SelectionSet Int AniList.Object.UserStatistics
count =
    Object.selectionForField "Int" "count" [] Decode.int


meanScore : SelectionSet Float AniList.Object.UserStatistics
meanScore =
    Object.selectionForField "Float" "meanScore" [] Decode.float


standardDeviation : SelectionSet Float AniList.Object.UserStatistics
standardDeviation =
    Object.selectionForField "Float" "standardDeviation" [] Decode.float


minutesWatched : SelectionSet Int AniList.Object.UserStatistics
minutesWatched =
    Object.selectionForField "Int" "minutesWatched" [] Decode.int


episodesWatched : SelectionSet Int AniList.Object.UserStatistics
episodesWatched =
    Object.selectionForField "Int" "episodesWatched" [] Decode.int


chaptersRead : SelectionSet Int AniList.Object.UserStatistics
chaptersRead =
    Object.selectionForField "Int" "chaptersRead" [] Decode.int


volumesRead : SelectionSet Int AniList.Object.UserStatistics
volumesRead =
    Object.selectionForField "Int" "volumesRead" [] Decode.int


type alias FormatsOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


formats :
    (FormatsOptionalArguments -> FormatsOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserFormatStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
formats fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "formats" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias StatusesOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


statuses :
    (StatusesOptionalArguments -> StatusesOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserStatusStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
statuses fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "statuses" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias ScoresOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


scores :
    (ScoresOptionalArguments -> ScoresOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserScoreStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
scores fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "scores" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias LengthsOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


lengths :
    (LengthsOptionalArguments -> LengthsOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserLengthStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
lengths fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "lengths" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias ReleaseYearsOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


releaseYears :
    (ReleaseYearsOptionalArguments -> ReleaseYearsOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserReleaseYearStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
releaseYears fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "releaseYears" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias StartYearsOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


startYears :
    (StartYearsOptionalArguments -> StartYearsOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserStartYearStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
startYears fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "startYears" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias GenresOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


genres :
    (GenresOptionalArguments -> GenresOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserGenreStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
genres fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "genres" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias TagsOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


tags :
    (TagsOptionalArguments -> TagsOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserTagStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
tags fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "tags" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias CountriesOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


countries :
    (CountriesOptionalArguments -> CountriesOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserCountryStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
countries fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "countries" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias VoiceActorsOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


voiceActors :
    (VoiceActorsOptionalArguments -> VoiceActorsOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserVoiceActorStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
voiceActors fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "voiceActors" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias StaffOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


staff :
    (StaffOptionalArguments -> StaffOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserStaffStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
staff fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "staff" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)


type alias StudiosOptionalArguments =
    { limit : OptionalArgument Int
    , sort : OptionalArgument (List (Maybe AniList.Enum.UserStatisticsSort.UserStatisticsSort))
    }


studios :
    (StudiosOptionalArguments -> StudiosOptionalArguments)
    -> SelectionSet decodesTo AniList.Object.UserStudioStatistic
    -> SelectionSet (Maybe (List (Maybe decodesTo))) AniList.Object.UserStatistics
studios fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { limit = Absent, sort = Absent }

        optionalArgs____ =
            [ Argument.optional "limit" filledInOptionals____.limit Encode.int, Argument.optional "sort" filledInOptionals____.sort (Encode.enum AniList.Enum.UserStatisticsSort.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "studios" optionalArgs____ object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)
