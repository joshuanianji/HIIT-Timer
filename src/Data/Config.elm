module Data.Config exposing (Data, decodeLocalStorage, default, encode, fromLocalStorage)

-- the data the Config module stores (accessible to the App data as well)
-- helpful to put JSON decode and encode stuff without clogging up the Modules.Config.elm file

import Data.Duration as Duration
import Data.LocalStorageConfig as LocalStorageConfig
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Modules.Exercise as Exercise
import Modules.Set as Set exposing (Set)
import Modules.TimeInput as TimeInput exposing (TimeInput)


type alias Data =
    { exerciseInput : TimeInput
    , breakInput : TimeInput
    , setBreakInput : TimeInput
    , countdown : Bool
    , countdownInput : TimeInput
    , set : Dict Int Set

    -- so we won't get duplicate set positions - the set counter NEVER decreases
    , setCounter : Int
    , error : Maybe String
    }



-- from local storage


decodeLocalStorage : Json.Encode.Value -> Result Json.Decode.Error (Maybe LocalStorageConfig.Data)
decodeLocalStorage =
    let
        configStorageDecoder =
            Json.Decode.oneOf
                [ Json.Decode.null Nothing
                , LocalStorageConfig.decode
                    |> Json.Decode.map Just
                ]
    in
    Json.Decode.decodeValue configStorageDecoder


fromLocalStorage : LocalStorageConfig.Data -> Data
fromLocalStorage lsConfig =
    let
        fromLocalStorageSet set =
            Set.fromData
                { exercises = Dict.map (always Exercise.fromData) set.exercises
                , exerciseCounter = set.exerciseCounter
                , repeat = set.repeat
                , position = set.position
                , name = set.name
                , expanded = False
                }
    in
    { exerciseInput = TimeInput.init lsConfig.exerciseInput
    , breakInput = TimeInput.init lsConfig.breakInput
    , setBreakInput = TimeInput.init lsConfig.setBreakInput
    , countdown = lsConfig.countdown
    , countdownInput = TimeInput.init lsConfig.countdownInput
    , set = Dict.map (always fromLocalStorageSet) lsConfig.set
    , setCounter = lsConfig.setCounter
    , error = Nothing
    }



-- to local storage
-- helping me save the ENTIRE config object to local storage aHHHH


encode : Data -> Json.Encode.Value
encode data =
    let
        fromExercise _ exercise =
            Exercise.getData exercise

        fromSet _ set =
            let
                setData =
                    Set.getData set
            in
            { exercises = Dict.map fromExercise setData.exercises
            , exerciseCounter = setData.exerciseCounter
            , repeat = setData.repeat
            , position = setData.position
            , name = setData.name
            , expanded = setData.expanded
            }
    in
    { exerciseInput = Duration.toSeconds <| TimeInput.getDuration data.exerciseInput
    , breakInput = Duration.toSeconds <| TimeInput.getDuration data.breakInput
    , setBreakInput = Duration.toSeconds <| TimeInput.getDuration data.setBreakInput
    , countdown = data.countdown
    , countdownInput = Duration.toSeconds <| TimeInput.getDuration data.countdownInput
    , set = Dict.map fromSet data.set
    , setCounter = data.setCounter
    }
        |> LocalStorageConfig.encode


default : Data
default =
    let
        -- only one set lol
        setDict =
            Dict.fromList [ ( 1, Set.init 1 ) ]
    in
    { exerciseInput = TimeInput.init 60
    , breakInput = TimeInput.init 15
    , setBreakInput = TimeInput.init 60
    , countdown = True
    , countdownInput = TimeInput.init 5
    , set = setDict
    , setCounter = 1
    , error = Nothing
    }
