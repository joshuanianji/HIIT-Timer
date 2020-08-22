module Data.Workout exposing
    ( Workout
    , decode
    , decoder
    , default
    , encode
    , sanitizeSets
    , totalTime
    )

-- | Data for a singular workout

import Data.Duration as Duration exposing (Duration)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Modules.Set as Set exposing (Set)
import Util
import View.TimeInput as TimeInput exposing (TimeInput)


type Workout
    = Workout Data


type alias Data =
    { exerciseInput : TimeInput
    , breakInput : TimeInput
    , setBreakInput : TimeInput
    , countdown : Bool
    , countdownInput : TimeInput
    , sets : Dict Int Set

    -- so we won't get duplicate set positions - the set counter NEVER decreases
    , setCounter : Int

    -- config for sounds
    , speak : Bool
    , sounds : Bool
    }



-- JSON DECODE


decode : Encode.Value -> Result Decode.Error Workout
decode =
    Decode.decodeValue decoder


decoder : Decoder Workout
decoder =
    let
        timeInputDecoder =
            Decode.map TimeInput.init Decode.int
    in
    Decode.succeed Data
        |> Pipeline.required "exerciseInput" timeInputDecoder
        |> Pipeline.required "breakInput" timeInputDecoder
        |> Pipeline.required "setBreakInput" timeInputDecoder
        |> Pipeline.required "countdown" Decode.bool
        |> Pipeline.required "countdownInput" timeInputDecoder
        |> Pipeline.required "set" (Util.dictIntDecoder Set.decoder)
        |> Pipeline.required "setCounter" Decode.int
        |> Pipeline.optional "speak" Decode.bool False
        |> Pipeline.optional "sounds" Decode.bool True
        |> Decode.map Workout



-- JSON ENCODE


encode : Workout -> Encode.Value
encode (Workout data) =
    let
        encodeTimeInput =
            TimeInput.getDuration >> Duration.toSeconds >> Encode.int
    in
    Encode.object
        [ ( "exerciseInput", encodeTimeInput data.exerciseInput )
        , ( "breakInput", encodeTimeInput data.breakInput )
        , ( "setBreakInput", encodeTimeInput data.setBreakInput )
        , ( "countdown", Encode.bool data.countdown )
        , ( "countdownInput", encodeTimeInput data.countdownInput )
        , ( "set", Encode.dict String.fromInt Set.encode data.sets )
        , ( "setCounter", Encode.int data.setCounter )
        , ( "speak", Encode.bool data.speak )
        , ( "sounds", Encode.bool data.sounds )
        ]


default : Workout
default =
    let
        -- only one set lol
        setDict =
            Dict.fromList [ ( 1, Set.init 1 ) ]
    in
    Workout
        { exerciseInput = TimeInput.init 60
        , breakInput = TimeInput.init 15
        , setBreakInput = TimeInput.init 60
        , countdown = True
        , countdownInput = TimeInput.init 5
        , sets = setDict
        , setCounter = 1
        , speak = False
        , sounds = True
        }



-- HELPERS


totalTime : Workout -> Duration
totalTime (Workout data) =
    let
        setsDuration =
            Dict.toList data.sets
                |> List.map Tuple.second
                |> List.map
                    (Set.totalTime
                        { exerciseDuration = TimeInput.getDuration data.exerciseInput
                        , breakDuration = TimeInput.getDuration data.breakInput
                        }
                    )
                |> List.foldl Duration.add (Duration.init 0)

        breaksDuration =
            TimeInput.getDuration data.setBreakInput
                |> Duration.multiply (Dict.size data.sets - 1)
                |> Duration.clampBelow (Duration.init 0)

        countdownDuration =
            if data.countdown then
                TimeInput.getDuration data.countdownInput

            else
                Duration.init 0
    in
    [ setsDuration, breaksDuration, countdownDuration ]
        |> List.foldl Duration.add (Duration.init 0)


sanitizeSets : Workout -> Workout
sanitizeSets (Workout data) =
    Workout
        { data
            | sets =
                Dict.toList data.sets
                    |> List.indexedMap
                        (\n ( _, e ) -> ( n, Set.updatePosition n e ))
                    |> Dict.fromList
            , setCounter = Dict.size data.sets
        }
