module Data.LocalStorageConfig exposing (Data, decode, encode)

{-| types of all of our config data that's compatible with JSON and have to be stored
-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


type alias Data =
    { exerciseInput : Int
    , breakInput : Int
    , setBreakInput : Int
    , countdown : Bool
    , countdownInput : Int
    , sets : Dict Int Set
    , setCounter : Int
    , speak : Bool
    , sounds : Bool
    }


type alias Set =
    { name : String
    , position : Int
    , expanded : Bool
    , repeat : Int
    , exerciseCounter : Int
    , exercises : Dict Int Exercise
    }



-- kinda the exact same as the actual implementation lol


type alias Exercise =
    { position : Int
    , name : String
    }



-- ENCODE


encode : Data -> Encode.Value
encode data =
    Encode.object
        [ ( "exerciseInput", Encode.int data.exerciseInput )
        , ( "breakInput", Encode.int data.breakInput )
        , ( "setBreakInput", Encode.int data.setBreakInput )
        , ( "countdown", Encode.bool data.countdown )
        , ( "countdownInput", Encode.int data.countdownInput )
        , ( "set", Encode.dict String.fromInt encodeSet data.sets )
        , ( "setCounter", Encode.int data.setCounter )
        , ( "speak", Encode.bool data.speak )
        , ( "sounds", Encode.bool data.sounds )
        ]


encodeSet : Set -> Encode.Value
encodeSet set =
    Encode.object
        [ ( "name", Encode.string set.name )
        , ( "position", Encode.int set.position )
        , ( "expanded", Encode.bool set.expanded )
        , ( "repeat", Encode.int set.repeat )
        , ( "exerciseCounter", Encode.int set.exerciseCounter )
        , ( "exercises", Encode.dict String.fromInt encodeExercise set.exercises )
        ]


encodeExercise : Exercise -> Encode.Value
encodeExercise exercise =
    Encode.object
        [ ( "position", Encode.int exercise.position )
        , ( "name", Encode.string exercise.name )
        ]



-- DECODE


decode : Decoder Data
decode =
    Decode.succeed Data
        |> Pipeline.required "exerciseInput" Decode.int
        |> Pipeline.required "breakInput" Decode.int
        |> Pipeline.required "setBreakInput" Decode.int
        |> Pipeline.required "countdown" Decode.bool
        |> Pipeline.required "countdownInput" Decode.int
        |> Pipeline.required "set" (dictIntDecoder setDecoder)
        |> Pipeline.required "setCounter" Decode.int
        |> Pipeline.optional "speak" Decode.bool False
        |> Pipeline.optional "sounds" Decode.bool True


setDecoder : Decoder Set
setDecoder =
    Decode.succeed Set
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "position" Decode.int
        |> Pipeline.required "expanded" Decode.bool
        |> Pipeline.required "repeat" Decode.int
        |> Pipeline.required "exerciseCounter" Decode.int
        |> Pipeline.required "exercises" (dictIntDecoder exerciseDecoder)


exerciseDecoder : Decoder Exercise
exerciseDecoder =
    Decode.succeed Exercise
        |> Pipeline.required "position" Decode.int
        |> Pipeline.required "name" Decode.string


dictIntDecoder : Decoder a -> Decoder (Dict Int a)
dictIntDecoder decoder =
    let
        helper acc list =
            case list of
                [] ->
                    Decode.succeed acc

                ( sk, v ) :: xs ->
                    case String.toInt sk of
                        Nothing ->
                            Decode.fail ("failed to convert to int: " ++ sk)

                        Just i ->
                            helper (acc ++ [ ( i, v ) ]) xs
    in
    Decode.keyValuePairs decoder
        |> Decode.andThen (helper [])
        |> Decode.map Dict.fromList
