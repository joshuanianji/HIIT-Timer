module Modules.Config exposing (Config, Msg, encode, getData, init, update, view)

import Colours
import Data.Config as Data
import Data.Duration as Duration exposing (Duration)
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icon
import Json.Decode
import Json.Encode
import Modules.Exercise as Exercise
import Modules.Set as Set
import Modules.TimeInput as TimeInput
import Util


type Config
    = Config Data.Data



-- INIT AND JSON STUFF


init : Json.Encode.Value -> Config
init localStorageValue =
    let
        decodeLocalStorageAttempt =
            Data.decodeLocalStorage localStorageValue

        ( actualData, mErr ) =
            case decodeLocalStorageAttempt of
                -- there was no config stored in the first place
                Ok Nothing ->
                    ( Data.default, Nothing )

                -- success
                Ok (Just config) ->
                    ( Data.fromLocalStorage config, Nothing )

                -- failure to decode
                Err jsonErr ->
                    ( Data.default, Just <| Json.Decode.errorToString jsonErr )
    in
    Config { actualData | error = mErr }



-- helpers


encode : Config -> Json.Encode.Value
encode (Config data) =
    Data.encode data


getData : Config -> Data.Data
getData (Config data) =
    data



-- internal helpers


totalTime : Data.Data -> Duration
totalTime data =
    Dict.toList data.sets
        |> List.map Tuple.second
        |> List.map
            (Set.totalTime
                { exerciseDuration = TimeInput.getDuration data.exerciseInput
                , breakDuration = TimeInput.getDuration data.breakInput
                }
            )
        |> List.intersperse (TimeInput.getDuration data.setBreakInput)
        |> List.foldl Duration.add (Duration.init 0)



-- VIEW


view : Config -> Element Msg
view (Config data) =
    Element.column
        [ Element.width (Element.fill |> Element.maximum 1000)
        , Element.centerX
        , Element.height Element.fill
        , Element.padding 32
        , Element.spacing 48
        ]
        [ Util.viewIcon
            { icon = Icon.settings
            , color = Colours.sky
            , size = 50
            , msg = Nothing
            }
            |> Element.el [ Element.centerX ]
        , data.error
            |> Maybe.map Element.text
            |> Maybe.withDefault Element.none

        -- actual settings stuff
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Element.column
                [ Element.centerX
                , Element.spacing 64
                ]
                [ Element.el
                    [ Element.onRight <|
                        Element.el
                            [ Element.centerY ]
                        <|
                            TimeInput.view
                                { updateInput = UpdateInput Exercise
                                , updateFocus = UpdateFocus Exercise
                                , displayText = Nothing
                                }
                                data.exerciseInput
                    , Element.text "Exercise Duration:"
                        |> Element.el [ Element.centerY ]
                        |> Element.el
                            [ Font.light
                            , Element.height (Element.px 50)
                            , Element.centerY
                            ]
                        |> Element.onLeft
                    , Element.centerX
                    ]
                    Element.none
                , Element.el
                    [ Element.onRight <|
                        Element.el
                            [ Element.centerY ]
                        <|
                            TimeInput.view
                                { updateInput = UpdateInput Break
                                , updateFocus = UpdateFocus Break
                                , displayText = Nothing
                                }
                                data.breakInput
                    , Element.text "Break Between Exercises:"
                        |> Element.el [ Element.centerY ]
                        |> Element.el
                            [ Font.light
                            , Element.height (Element.px 50)
                            , Element.centerY
                            ]
                        |> Element.onLeft
                    , Element.centerX
                    ]
                    Element.none
                , Element.el
                    [ Element.onRight <|
                        Element.el
                            [ Element.centerY ]
                        <|
                            TimeInput.view
                                { updateInput = UpdateInput SetBreak
                                , updateFocus = UpdateFocus SetBreak
                                , displayText = Nothing
                                }
                                data.setBreakInput
                    , Element.text "Break Between Sets:"
                        |> Element.el [ Element.centerY ]
                        |> Element.el
                            [ Font.light
                            , Element.height (Element.px 50)
                            , Element.centerY
                            ]
                        |> Element.onLeft
                    , Element.centerX
                    ]
                    Element.none

                -- countdown
                , Element.row
                    [ Element.spacing 8
                    , Element.centerX
                    ]
                    [ Input.checkbox
                        [ Font.light
                        , Element.padding 4
                        ]
                        { onChange = ToggleCountdown
                        , icon =
                            \on ->
                                if on then
                                    Util.viewIcon
                                        { icon = Icon.checkSquare
                                        , color = Colours.grass
                                        , size = 30
                                        , msg = Nothing
                                        }

                                else
                                    Util.viewIcon
                                        { icon = Icon.xSquare
                                        , color = Colours.sunset
                                        , size = 30
                                        , msg = Nothing
                                        }
                        , checked = data.countdown
                        , label = Input.labelLeft [ Element.padding 8, Element.centerY ] <| Element.text "Countdown:"
                        }
                        |> Element.el
                            [ Element.centerX ]
                    , if data.countdown then
                        data.countdownInput
                            |> TimeInput.view
                                { updateInput = UpdateInput Countdown
                                , updateFocus = UpdateFocus Countdown
                                , displayText = Nothing
                                }

                      else
                        Element.el
                            [ Element.width <| Element.px 188
                            , Element.padding 4
                            ]
                            Element.none
                    ]
                ]
            ]

        -- set stuff
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 18
            ]
            [ -- total time
              Element.row
                [ Element.spacing 2
                , Element.centerX
                , Font.light
                ]
                [ Element.text "Total time: "
                , totalTime data
                    |> Duration.viewFancy
                    |> Element.el
                        [ Font.color Colours.sunflower ]
                ]
            , Dict.toList
                data.sets
                |> List.map
                    (\( _, set ) ->
                        Set.view
                            { onNewExercise = NewElement

                            -- exercise position then set position
                            , onDeleteExercise = DeleteElement
                            , onDelete = DeleteSet
                            , onUpdateRepeat = NewSetRepeat
                            , toggleExpand = ToggleSetExpand
                            , updateName = UpdateSetName
                            , updateExerciseName = UpdateExerciseName
                            , exerciseDuration = TimeInput.getDuration data.exerciseInput
                            , breakDuration = TimeInput.getDuration data.breakInput
                            }
                            set
                    )
                |> List.intersperse (Exercise.breakView <| TimeInput.getDuration data.setBreakInput)
                |> Element.column
                    [ Element.spacing 32
                    , Element.width Element.fill
                    ]

            -- add set
            , Element.el
                [ Element.alignBottom
                , Element.centerX
                ]
                (Util.viewIcon
                    { icon = Icon.plus
                    , color = Colours.sunflower
                    , size = 40
                    , msg = Just AddSet
                    }
                    |> Element.el
                        [ Element.alignBottom
                        , Element.centerX
                        , Background.color Colours.white
                        ]
                )
            ]
        ]



-- UPDATE
-- either the exercise, break, countdown or setBreak


type Input
    = Exercise
    | Break
    | SetBreak
    | Countdown


type Msg
    = UpdateInput Input String
    | UpdateFocus Input Bool
    | NewElement Int
    | DeleteElement Int Int
    | NewSetRepeat Int Int
    | DeleteSet Int
    | AddSet
    | ToggleSetExpand Int
    | UpdateSetName Int String
    | UpdateExerciseName Int Int String
    | ToggleCountdown Bool


update : Msg -> Config -> Config
update msg (Config data) =
    case msg of
        UpdateInput Exercise newVal ->
            Config { data | exerciseInput = TimeInput.updateInput data.exerciseInput newVal }

        UpdateFocus Exercise isFocused ->
            Config { data | exerciseInput = TimeInput.updateFocus data.exerciseInput isFocused }

        UpdateInput Break newVal ->
            Config { data | breakInput = TimeInput.updateInput data.breakInput newVal }

        UpdateFocus Break isFocused ->
            Config { data | breakInput = TimeInput.updateFocus data.breakInput isFocused }

        UpdateInput SetBreak newVal ->
            Config { data | setBreakInput = TimeInput.updateInput data.setBreakInput newVal }

        UpdateFocus SetBreak isFocused ->
            Config { data | setBreakInput = TimeInput.updateFocus data.setBreakInput isFocused }

        UpdateInput Countdown newVal ->
            Config { data | countdownInput = TimeInput.updateInput data.countdownInput newVal }

        UpdateFocus Countdown isFocused ->
            Config { data | countdownInput = TimeInput.updateFocus data.countdownInput isFocused }

        ToggleCountdown bool ->
            Config { data | countdown = bool }

        NewElement setPos ->
            Config
                { data
                    | sets =
                        Dict.update
                            setPos
                            (Maybe.map Set.newExercise)
                            data.sets
                }

        DeleteElement setPos elemPos ->
            Config
                { data
                    | sets =
                        Dict.update
                            setPos
                            (Maybe.map <| (Set.deleteExercise elemPos >> Set.sanitizeExercises))
                            data.sets
                }

        NewSetRepeat setPos repeat ->
            Config
                { data
                    | sets =
                        Dict.update
                            setPos
                            (Maybe.map <| Set.updateRepeat repeat)
                            data.sets
                }

        DeleteSet setPos ->
            Config <| Data.sanitizeSets { data | sets = Dict.remove setPos data.sets }

        AddSet ->
            let
                newN =
                    data.setCounter + 1
            in
            Config
                { data
                    | sets = Dict.insert newN (Set.init newN) data.sets
                    , setCounter = newN
                }

        ToggleSetExpand setPos ->
            Config { data | sets = Dict.update setPos (Maybe.map Set.toggleExpand) data.sets }

        UpdateSetName setPos newName ->
            Config { data | sets = Dict.update setPos (Maybe.map <| Set.updateName newName) data.sets }

        UpdateExerciseName setPos exercisePos newName ->
            Config { data | sets = Dict.update setPos (Maybe.map <| Set.updateExerciseName exercisePos newName) data.sets }
