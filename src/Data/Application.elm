module Data.Application exposing (AppState(..), Data, TimeBlock(..), decreaseTimeBlock, fromConfig)

import Data.Config as Config
import Data.Duration as Duration
import Dict
import List.Nonempty exposing (Nonempty(..))
import Modules.Set as Set
import Modules.TimeInput as TimeInput


type alias Data =
    { playing : Bool
    , state : AppState
    }


type AppState
    = Starting (Nonempty TimeBlock) -- the exercises to hold on to lol
    | InProgress (Nonempty TimeBlock)
    | Finished


type TimeBlock
    = Break Int Int
    | CountDown Int Int
    | Exercise
        { setName : String
        , name : String
        , duration : Int
        , secsLeft : Int
        }



-- if the timeblock ends we return Nothing


decreaseTimeBlock : TimeBlock -> Maybe TimeBlock
decreaseTimeBlock tb =
    case tb of
        Break curr total ->
            if curr <= 1 then
                Nothing

            else
                Just <| Break (curr - 1) total

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

        exercises =
            configData.set
                |> Dict.map (\_ -> Set.getEssentials (TimeInput.getDuration configData.exerciseInput))
                |> Dict.toList
                |> List.map Tuple.second
                |> List.filterMap
                    (\( setName, exerciseList ) ->
                        case exerciseList of
                            [] ->
                                Nothing

                            x :: xs ->
                                Nonempty x xs
                                    |> List.Nonempty.map
                                        (\( name, duration ) ->
                                            Exercise
                                                { setName = setName
                                                , name = name
                                                , duration = Duration.toSeconds duration
                                                , secsLeft = Duration.toSeconds duration
                                                }
                                        )
                                    |> intersperseNonempty (Break breakSecs breakSecs)
                                    |> Just
                    )
                |> List.Nonempty.fromList
                |> Maybe.map
                    (Break setBreakSecs setBreakSecs
                        |> List.Nonempty.fromElement
                        |> intersperseNonempty
                    )
                |> Maybe.map List.Nonempty.concat

        state =
            case exercises of
                Just blocks ->
                    if configData.countdown then
                        blocks
                            |> List.Nonempty.cons (CountDown countdownSecs countdownSecs)
                            |> Starting

                    else
                        Starting blocks

                -- no elements
                Nothing ->
                    Finished
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
