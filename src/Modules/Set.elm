module Modules.Set exposing
    ( Set
    , deleteExercise
    , fromData
    , getData
    , getEssentials
    , init
    , newExercise
    , toggleExpand
    , totalTime
    , updateExerciseName
    , updateName
    , updateRepeat
    , view
    )

-- A collection of exercises

import Colours
import Data.Duration as Duration exposing (Duration)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icon
import Modules.Exercise as Exercise exposing (Exercise)
import Util


type Set
    = Set Data



-- internal data - everything that isn't dependant on external data


type alias Data =
    { exercises : Dict Int Exercise

    -- ensures we don't get duplicate numbers when we delete elements
    , exerciseCounter : Int

    -- how many times we repeat the set
    , repeat : Int

    -- same things as exercise data
    , position : Int
    , name : String

    -- toggle
    , expanded : Bool
    }



-- user puts these in for the view function


type alias Options msg =
    { -- the int is the position
      onNewExercise : Int -> msg

    -- set position, then exercise num
    , onDeleteExercise : Int -> Int -> msg

    -- delete the entire set
    , onDelete : Int -> msg

    -- updates how may times the set will repeat
    -- first is the set position
    , onUpdateRepeat : Int -> Int -> msg

    -- int is the position again
    , toggleExpand : Int -> msg

    -- AGAIN, the position ofthe set is first, followed by the new name
    , updateName : Int -> String -> msg

    -- set position, exercise position, newExercise name
    , updateExerciseName : Int -> Int -> String -> msg

    -- data we need to get from external sources (that can also update)
    , exerciseDuration : Duration
    , breakDuration : Duration
    }


init : Int -> Set
init n =
    let
        exercises =
            List.map2 Tuple.pair (List.range 1 4) (List.map Exercise.init <| List.range 1 4)
                |> Dict.fromList
    in
    Set
        { exercises = exercises
        , exerciseCounter = 4
        , repeat = 2
        , position = n
        , name = "Set " ++ String.fromInt n
        , expanded = False
        }



-- helpers
-- to a list of exercise name and duration (used in application)


getEssentials : Duration -> Set -> ( String, List ( String, Duration ) )
getEssentials exerciseDuration (Set data) =
    data.exercises
        |> Dict.map (\_ -> Exercise.essentials exerciseDuration)
        |> Dict.toList
        |> List.map Tuple.second
        |> (\l -> ( data.name, l ))



-- get total time


totalTime :
    { exerciseDuration : Duration
    , breakDuration : Duration
    }
    -> Set
    -> Duration
totalTime options (Set data) =
    Duration.add (Duration.times (getTimeWithoutRepeats options (Set data)) data.repeat) (Duration.times options.breakDuration (data.repeat - 1))


getTimeWithoutRepeats :
    { exerciseDuration : Duration
    , breakDuration : Duration
    }
    -> Set
    -> Duration
getTimeWithoutRepeats options set =
    getEssentials options.exerciseDuration set
        |> Tuple.second
        |> List.map Tuple.second
        -- adding in break durations
        |> List.intersperse options.breakDuration
        |> List.foldl Duration.add (Duration.init 0)



-- for localstorage stuff


getData : Set -> Data
getData (Set data) =
    data


fromData : Data -> Set
fromData =
    Set



-- VIEW


view : Options msg -> Set -> Element msg
view options (Set data) =
    Element.column
        [ Element.width Element.fill
        , Element.padding 16
        , Element.spacing 32
        , Border.width 1
        , Border.color Colours.sunflower
        , Border.rounded 4
        ]
        [ -- title stuff
          Element.row
            [ Element.width Element.fill ]
            [ Element.column
                [ Element.spacing 16
                , Element.width Element.fill
                ]
                [ Input.text
                    [ Element.width (Element.fill |> Element.maximum 400)
                    , Font.size 36
                    , Font.color Colours.sunflower
                    , Border.color Colours.white
                    , Element.padding 0
                    , Element.mouseOver
                        [ Border.color Colours.sunflower ]
                    ]
                    { onChange = options.updateName data.position
                    , text = data.name
                    , placeholder = Nothing
                    , label = Input.labelHidden "New name for set"
                    }

                -- number of repeats
                , Element.row
                    []
                    [ Input.text
                        [ Font.light
                        ]
                        { onChange =
                            String.filter Char.isDigit
                                >> String.toInt
                                >> Maybe.withDefault 1
                                >> options.onUpdateRepeat data.position
                        , text = String.fromInt data.repeat
                        , placeholder = Nothing
                        , label = Input.labelLeft [ Font.light ] <| Element.text "Number of Repeats:"
                        }
                    , Util.viewIcon
                        { icon = Icon.repeat
                        , color = Colours.sky
                        , size = 25
                        , msg = Nothing
                        }
                    ]

                -- total times
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing 8
                    ]
                    [ Element.row
                        [ -- total time of a set
                          Font.light
                        , Element.spacing 2
                        ]
                        [ getTimeWithoutRepeats
                            { exerciseDuration = options.exerciseDuration
                            , breakDuration = options.breakDuration
                            }
                            (Set data)
                            |> Duration.viewFancy
                            |> Element.el
                                [ Font.color Colours.sunflower ]
                        , Element.text " for "
                        , Dict.size data.exercises
                            |> String.fromInt
                            |> Element.text
                            |> Element.el [ Font.color Colours.sunflower ]
                        , Element.el [ Font.color Colours.sunflower ] <|
                            Element.text
                                (if Dict.size data.exercises == 1 then
                                    " exercise."

                                 else
                                    " exercises."
                                )
                        ]

                    -- total time including repeats
                    , if data.repeat <= 1 then
                        -- no point in repeating the same info
                        Element.none

                      else
                        Element.row
                            [ Font.light
                            , Element.spacing 2
                            ]
                            [ Element.text "Total time: "
                            , totalTime
                                { exerciseDuration = options.exerciseDuration
                                , breakDuration = options.breakDuration
                                }
                                (Set data)
                                |> Duration.viewFancy
                                |> Element.el
                                    [ Font.color Colours.sunflower ]
                            , Element.text ", with "
                            , data.repeat
                                - 1
                                |> String.fromInt
                                |> Element.text
                                |> Element.el [ Font.color Colours.sunflower ]
                            , Element.el [ Font.color Colours.sunflower ] <|
                                Element.text
                                    (if data.repeat == 2 then
                                        " break."

                                     else
                                        " breaks."
                                    )
                            ]
                    ]
                ]

            -- icons on the right
            , Element.column
                [ Element.spacing 8 ]
                [ Util.viewIcon
                    { icon = Icon.trash
                    , color = Colours.sunset
                    , size = 25
                    , msg = Just <| options.onDelete data.position
                    }
                    |> Element.el
                        [ Element.alignRight
                        , Element.centerY
                        ]
                , Util.viewIcon
                    { icon =
                        if data.expanded then
                            Icon.chevronUp

                        else
                            Icon.chevronDown
                    , color = Colours.sky
                    , size = 25
                    , msg = Just <| options.toggleExpand data.position
                    }
                    |> Element.el
                        [ Element.alignRight
                        , Element.centerY
                        ]
                ]
            ]

        -- exercises
        , if data.expanded then
            Element.column
                [ Element.spacing 16
                , Element.width Element.fill
                ]
                [ Dict.toList
                    data.exercises
                    |> List.map
                        (\( _, e ) ->
                            Exercise.view
                                { onDelete = options.onDeleteExercise data.position
                                , updateName = options.updateExerciseName data.position
                                , duration = options.exerciseDuration
                                }
                                e
                        )
                    |> List.intersperse (Exercise.breakView options.breakDuration)
                    |> Element.column
                        [ Element.spacing 8
                        , Element.width Element.fill
                        ]
                , Element.el
                    [ Element.alignBottom
                    , Element.centerX
                    ]
                    (Util.viewIcon
                        { icon = Icon.plus
                        , color = Colours.sunset
                        , size = 30
                        , msg = Just <| options.onNewExercise data.position
                        }
                        |> Element.el
                            [ Element.alignBottom
                            , Element.centerX
                            , Background.color Colours.white
                            ]
                    )
                ]

          else
            Element.none
        ]



-- UPDATE


newExercise : Set -> Set
newExercise (Set data) =
    let
        newN =
            data.exerciseCounter + 1
    in
    Set
        { data
            | exercises =
                Dict.insert newN (Exercise.init newN) data.exercises
            , exerciseCounter = newN
        }


updateExerciseName : Int -> String -> Set -> Set
updateExerciseName pos newName (Set data) =
    let
        newExercises =
            Dict.update pos (Maybe.map <| Exercise.updateName newName) data.exercises
    in
    Set
        { data | exercises = newExercises }


deleteExercise : Int -> Set -> Set
deleteExercise position (Set data) =
    Set { data | exercises = Dict.remove position data.exercises }


updateRepeat : Int -> Set -> Set
updateRepeat newRepeat (Set data) =
    Set { data | repeat = newRepeat }


toggleExpand : Set -> Set
toggleExpand (Set data) =
    Set { data | expanded = not data.expanded }


updateName : String -> Set -> Set
updateName newName (Set data) =
    Set { data | name = newName }
