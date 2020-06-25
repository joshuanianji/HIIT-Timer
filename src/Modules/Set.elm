module Modules.Set exposing
    ( Set
    , SetEssentials
    , deleteExercise
    , fromData
    , getData
    , getEssentials
    , init
    , newExercise
    , sanitizeExercises
    , toggleExpand
    , totalTime
    , updateExerciseName
    , updateName
    , updatePosition
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
import Html.Attributes
import Modules.Exercise as Exercise exposing (Exercise)
import Util



---- TYPE ----


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

    -- copies the set and places the copy right below it
    , onCopy : Int -> msg

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

    -- DEVICEEE
    , device : Element.Device
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



---- HELPERS ----
-- to a list of exercise name, repeats and duration (used in application)


type alias SetEssentials =
    { name : String
    , repeats : Int
    , exercises : List ( String, Duration )
    }


getEssentials : Duration -> Set -> SetEssentials
getEssentials exerciseDuration (Set data) =
    data.exercises
        |> Dict.map (\_ -> Exercise.essentials exerciseDuration)
        |> Dict.toList
        |> List.map Tuple.second
        |> (\l -> SetEssentials data.name data.repeat l)



-- get total time


totalTime :
    { exerciseDuration : Duration
    , breakDuration : Duration
    }
    -> Set
    -> Duration
totalTime options (Set data) =
    if data.repeat == 0 then
        Duration.init 0

    else
        Duration.add (Duration.times (getTimeWithoutRepeats options (Set data)) data.repeat) (Duration.times options.breakDuration (data.repeat - 1))


getTimeWithoutRepeats :
    { exerciseDuration : Duration
    , breakDuration : Duration
    }
    -> Set
    -> Duration
getTimeWithoutRepeats options set =
    getEssentials options.exerciseDuration set
        |> .exercises
        |> List.map Tuple.second
        -- adding in break durations
        |> List.intersperse options.breakDuration
        |> List.foldl Duration.add (Duration.init 0)



-- to sanitize the exercises, we make the keys to the dictionary increasing in order again and change the exerciseCounter to the size of the dictionary. Note that this does not change the names in any way.
-- sanitation is nice to make the counter not go up indefinitely lol.


sanitizeExercises : Set -> Set
sanitizeExercises (Set data) =
    Set
        { data
            | exercises =
                Dict.toList data.exercises
                    |> List.indexedMap
                        (\n ( _, e ) -> ( n, Exercise.updatePosition n e ))
                    |> Dict.fromList
            , exerciseCounter = Dict.size data.exercises
        }



-- for config to sanitize set dictionary


updatePosition : Int -> Set -> Set
updatePosition n (Set data) =
    Set { data | position = n }



-- for localstorage stuff


getData : Set -> Data
getData (Set data) =
    data


fromData : Data -> Set
fromData =
    Set



---- VIEW ----


view : Options msg -> Set -> Element msg
view options (Set data) =
    let
        title =
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
                    ]
                ]

        repeats =
            let
                label =
                    Element.el [ Font.light ] <| Element.text "Number of Repeats:"

                input =
                    Input.text
                        [ Font.light
                        , Element.htmlAttribute <| Html.Attributes.type_ "number"
                        ]
                        { onChange =
                            String.filter Char.isDigit
                                >> String.toInt
                                >> Maybe.withDefault 0
                                >> options.onUpdateRepeat data.position
                        , text = String.fromInt data.repeat
                        , placeholder = Nothing
                        , label = Input.labelHidden "Number of Repeats:"
                        }

                repeatIcon =
                    Util.viewIcon
                        { icon = Icon.repeat
                        , color = Colours.sky
                        , size = 25
                        , msg = Nothing
                        }
            in
            if Util.isVerticalPhone options.device then
                Element.column
                    [ Element.spacing 4 ]
                    [ label
                    , Element.row
                        [ Element.spacing 4 ]
                        [ input, repeatIcon ]
                    ]

            else
                Element.row
                    [ Element.spacing 4 ]
                    [ label
                    , input
                    , repeatIcon
                    ]

        totalDurations =
            Element.column
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

        deleteSetIcon =
            Util.viewIcon
                { icon = Icon.trash
                , color = Colours.sunset
                , size = 25
                , msg = Just <| options.onDelete data.position
                }

        copySetIcon =
            Util.viewIcon
                { icon = Icon.copy
                , color = Colours.sky
                , size = 25
                , msg = Just <| options.onCopy data.position
                }
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 16
        , Element.spacing 32
        , Border.width 1
        , Border.color Colours.sunflower
        , Border.rounded 4
        ]
        [ if Util.isVerticalPhone options.device then
            -- completely vertical (also no tooltips)
            Element.column
                [ Element.spacing 24 ]
                [ title
                , repeats
                , totalDurations
                , Element.row
                    [ Element.spacing 8
                    , Element.centerX
                    ]
                    [ deleteSetIcon
                    , copySetIcon
                    ]
                ]

          else
            -- some horizontal
            Element.row
                [ Element.width Element.fill ]
                [ Element.column
                    [ Element.spacing 8
                    , Element.width Element.fill
                    ]
                    [ title
                    , repeats
                    , totalDurations
                    ]
                , Element.column
                    [ Element.spacing 8
                    , Element.alignRight
                    , Element.centerY
                    ]
                    [ deleteSetIcon
                        |> Util.withTooltip
                            { position = Util.Left
                            , content = "Delete Set"
                            }
                    , copySetIcon
                        |> Util.withTooltip
                            { position = Util.Left
                            , content = "Duplicate Set"
                            }
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

        -- expand toggle button
        , Util.viewIcon
            { icon =
                if data.expanded then
                    Icon.chevronUp

                else
                    Icon.chevronDown
            , color = Colours.sunflower
            , size = 25
            , msg = Just <| options.toggleExpand data.position
            }
            |> Util.withTooltip
                { position = Util.Top
                , content =
                    if data.expanded then
                        "Collapse Set"

                    else
                        "Expand Set"
                }
            |> Element.el
                [ Element.centerX ]
        ]



---- UPDATE HELPERS ----


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
