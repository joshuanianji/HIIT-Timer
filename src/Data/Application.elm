module Data.Application exposing (AppState(..), Data, ExerciseState(..), fromConfig)

import Data.Config as Config
import Data.Duration as Duration exposing (Duration)
import Dict
import List.Nonempty exposing (Nonempty)
import Modules.Set as Set
import Modules.TimeInput as TimeInput


type alias Data =
    { exercises : List ( String, List ( String, Duration ) )
    , playing : Bool
    , countdown : Maybe Duration
    , state : AppState
    }


type AppState
    = InProgress ExerciseState
    | Finished


type ExerciseState
    = CountingDown Int
    | Exercising Int Int (Nonempty ( String, List ( String, Duration ) )) -- set number, exercise number and list of sets and exercises



-- when we initialize the application we need to convert our config data into application data


fromConfig : Config.Data -> Data
fromConfig configData =
    let
        exercises =
            configData.set
                |> Dict.map (\_ -> Set.getEssentials (TimeInput.getDuration configData.exerciseInput))
                |> Dict.toList
                |> List.map Tuple.second

        ( countdown, state ) =
            if configData.countdown then
                let
                    countdownDuration =
                        TimeInput.getDuration configData.countdownInput
                in
                ( Just countdownDuration, InProgress <| CountingDown (Duration.toSeconds countdownDuration) )

            else
                ( Nothing
                , case exercises of
                    set :: tl ->
                        InProgress <| Exercising 1 1 (toNonEmptyList ( set, tl ))

                    [] ->
                        Finished
                )
    in
    { playing = False
    , exercises = exercises
    , countdown = countdown
    , state = state
    }



-- helper


toNonEmptyList : ( a, List a ) -> Nonempty a
toNonEmptyList ( hd, tl ) =
    case List.Nonempty.fromList tl of
        Nothing ->
            List.Nonempty.fromElement hd

        Just lst ->
            List.Nonempty.append (List.Nonempty.fromElement hd) lst
