module Data.Application exposing
    ( AppState(..)
    , Data
    , TimeBlock(..)
    , WorkoutData
    , WorkoutInfo
    , decreaseTimeBlock
    , fromConfig
    , timeLeft
    )

import Data.Config as Config
import Data.Duration as Duration exposing (Duration)
import Dict
import List.Nonempty exposing (Nonempty(..))
import Modules.Set as Set
import Set
import View.TimeInput as TimeInput



---- TYPE ----


type alias Data =
    { playing : Bool
    , state : AppState
    }


type AppState
    = Starting WorkoutData
    | InProgress WorkoutData
    | NeverStarted
    | Finished



-- data about the workout to easily keep track of in the application


type alias WorkoutData =
    { blocksLeft : Nonempty TimeBlock

    -- static data about our workout
    , info : WorkoutInfo
    }


type alias WorkoutInfo =
    { totalExercises : Int
    , totalSets : Int
    , totalTimeblocks : Int
    , totalTime : Duration
    }


type TimeBlock
    = ExerciseBreak Int Int
    | SetBreak Int Int
    | CountDown Int Int
    | Exercise
        { setName : String
        , name : String
        , duration : Int
        , secsLeft : Int
        }


timeLeft : TimeBlock -> Int
timeLeft block =
    case block of
        ExerciseBreak remaining _ ->
            remaining

        SetBreak remaining _ ->
            remaining

        CountDown remaining _ ->
            remaining

        Exercise data ->
            data.secsLeft



-- if the timeblock ends we return Nothing


decreaseTimeBlock : TimeBlock -> Maybe TimeBlock
decreaseTimeBlock tb =
    case tb of
        ExerciseBreak curr total ->
            if curr <= 1 then
                Nothing

            else
                Just <| ExerciseBreak (curr - 1) total

        SetBreak curr total ->
            if curr <= 1 then
                Nothing

            else
                Just <| SetBreak (curr - 1) total

        CountDown curr total ->
            if curr <= 1 then
                Nothing

            else
                Just <| CountDown (curr - 1) total

        Exercise dater ->
            if dater.secsLeft <= 1 then
                Nothing

            else
                Just <| Exercise { dater | secsLeft = dater.secsLeft - 1 }



-- when we initialize the application we need to convert our config data into application data


fromConfig : Config.Data -> Data
fromConfig configData =
    let
        breakSecs =
            Duration.toSeconds <| TimeInput.getDuration configData.breakInput

        setBreakSecs =
            Duration.toSeconds <| TimeInput.getDuration configData.setBreakInput

        countdownSecs =
            Duration.toSeconds <| TimeInput.getDuration configData.countdownInput

        -- flatten the sets into a nonempty list of exercises
        exercises =
            configData.sets
                |> Dict.toList
                |> List.map Tuple.second
                |> List.map
                    (\set ->
                        Set.getEssentials (TimeInput.getDuration configData.exerciseInput) set
                            |> (\setData ->
                                    case setData.exercises of
                                        [] ->
                                            []

                                        x :: xs ->
                                            Nonempty x xs
                                                |> List.Nonempty.map
                                                    (\( exerciseName, exerciseDuration ) ->
                                                        Exercise
                                                            { setName = setData.name
                                                            , name = exerciseName
                                                            , duration = Duration.toSeconds exerciseDuration
                                                            , secsLeft = Duration.toSeconds exerciseDuration
                                                            }
                                                    )
                                                |> intersperseNonempty (ExerciseBreak breakSecs breakSecs)
                                                |> List.repeat setData.repeats
                               )
                    )
                -- at this point we have a List (List (Nonempty TimeBlock))
                |> List.concat
                |> List.intersperse (List.Nonempty.fromElement <| SetBreak setBreakSecs setBreakSecs)
                |> List.Nonempty.fromList
                |> Maybe.map List.Nonempty.concat
                |> Maybe.map
                    -- add countdown
                    (\blocks ->
                        if configData.countdown then
                            List.Nonempty.cons (CountDown countdownSecs countdownSecs) blocks

                        else
                            blocks
                    )

        -- the static information about the workout
        setDictFold currSet acc =
            let
                set =
                    Set.getData currSet
            in
            acc + set.repeat * Dict.size set.exercises

        workoutInfo =
            { totalExercises = Dict.foldl (always setDictFold) 0 configData.sets
            , totalSets = Dict.size configData.sets
            , totalTimeblocks =
                Maybe.map List.Nonempty.length exercises
                    |> Maybe.withDefault 0
            , totalTime = Config.totalTime configData
            }

        state =
            case exercises of
                Just blocks ->
                    Starting
                        { blocksLeft = blocks
                        , info = workoutInfo
                        }

                -- no elements - never started the workout smh
                Nothing ->
                    NeverStarted
    in
    { playing = False
    , state = state
    }



-- internal helpers


intersperseNonempty : a -> Nonempty a -> Nonempty a
intersperseNonempty a l =
    case List.intersperse a (List.Nonempty.toList l) of
        [] ->
            List.Nonempty.fromElement a

        x :: xs ->
            Nonempty x xs
