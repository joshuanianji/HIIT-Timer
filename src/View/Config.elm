module View.Config exposing
    ( Config
    , Msg
    , encode
    , getData
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Colours
import Data.Config as Data
import Data.Duration as Duration exposing (Duration)
import Data.Flags as Flags exposing (Flags)
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
import Ports
import Util



---- TYPE ----


type Config
    = Config Model Data.Data


type alias Model =
    { device : Element.Device
    , saving : Bool
    }



-- INIT AND JSON STUFF


init : Flags -> Config
init flags =
    let
        decodeLocalStorageAttempt =
            Data.decodeLocalStorage flags.storedConfig

        ( data, mErr ) =
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

        actualData =
            { data | error = mErr }

        model =
            { device = Element.classifyDevice flags.windowSize
            , saving = False
            }
    in
    Config model { actualData | error = mErr }



-- helpers


encode : Config -> Json.Encode.Value
encode (Config _ data) =
    Data.encode data


getData : Config -> Data.Data
getData (Config _ data) =
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
        |> Duration.add
            (if data.countdown then
                TimeInput.getDuration data.countdownInput

             else
                Duration.init 0
            )



---- VIEW ----


view : Config -> Element Msg
view (Config model data) =
    let
        paddingX =
            case model.device.class of
                Element.Phone ->
                    4

                _ ->
                    32
    in
    Element.column
        [ Element.width (Element.fill |> Element.maximum 1000)
        , Element.centerX
        , Element.height Element.fill
        , Element.paddingXY paddingX 32
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
                , Element.spacing 12
                ]
                [ TimeInput.view
                    { updateInput = UpdateInput Exercise
                    , updateFocus = UpdateFocus Exercise
                    , displayText = Just "Exercise Duration:"
                    , device = model.device
                    }
                    data.exerciseInput
                , TimeInput.view
                    { updateInput = UpdateInput Break
                    , updateFocus = UpdateFocus Break
                    , displayText = Just "Break Between Exercises:"
                    , device = model.device
                    }
                    data.breakInput
                , TimeInput.view
                    { updateInput = UpdateInput SetBreak
                    , updateFocus = UpdateFocus SetBreak
                    , displayText = Just "Break Between Sets:"
                    , device = model.device
                    }
                    data.setBreakInput

                -- countdown
                , let
                    countdownLabel =
                        Input.checkbox
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
                                [ Element.centerY
                                , Element.centerX
                                ]

                    countdownInput =
                        if data.countdown then
                            data.countdownInput
                                |> TimeInput.view
                                    { updateInput = UpdateInput Countdown
                                    , updateFocus = UpdateFocus Countdown
                                    , displayText = Nothing
                                    , device = model.device
                                    }

                        else
                            Element.el
                                [ Element.width <| Element.px 188
                                , Element.padding 4
                                ]
                                Element.none
                  in
                  if Util.isVerticalPhone model.device then
                    -- orient vertically
                    Element.column
                        [ Element.spacing 8
                        , Element.centerX
                        ]
                        [ countdownLabel
                        , countdownInput
                        ]

                  else
                    -- orient horizontally (center the space between label and input)
                    Element.el
                        [ Element.onRight <|
                            Element.el [ Element.centerY ] countdownInput
                        , Element.onLeft countdownLabel
                        , Element.centerX
                        , Element.height (Element.px 64)
                        ]
                        Element.none
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
                            , onCopy = CopySet
                            , toggleExpand = ToggleSetExpand
                            , updateName = UpdateSetName
                            , updateExerciseName = UpdateExerciseName
                            , exerciseDuration = TimeInput.getDuration data.exerciseInput
                            , breakDuration = TimeInput.getDuration data.breakInput
                            , device = model.device
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

        -- If the settings is saved or not
        , Element.el
            [ Element.centerX ]
          <|
            if model.saving then
                Element.el
                    [ Font.light
                    , Font.color Colours.sunflower
                    ]
                <|
                    Element.text "saving..."

            else
                Element.el
                    [ Font.light
                    , Font.color Colours.grass
                    ]
                <|
                    Element.text "All Changes Saved to Local Storage "
        ]



---- UPDATE ----


type Msg
    = NewWindowSize Int Int
    | UpdateInput Input String
    | UpdateFocus Input Bool
    | NewElement Int
    | DeleteElement Int Int
    | NewSetRepeat Int Int
    | DeleteSet Int
    | AddSet
    | CopySet Int
    | ToggleSetExpand Int
    | UpdateSetName Int String
    | UpdateExerciseName Int Int String
    | ToggleCountdown Bool
    | ToLocalStorage -- save to local storage
    | StoreConfigSuccess -- when local storage succeeds



-- helps me differentiate between the different focuses


type Input
    = Exercise
    | Break
    | SetBreak
    | Countdown


update : Msg -> Config -> ( Config, Cmd Msg )
update msg (Config model data) =
    case msg of
        NewWindowSize width height ->
            ( Config { model | device = Element.classifyDevice <| Flags.WindowSize width height } data, Cmd.none )

        UpdateInput Exercise newVal ->
            ( Config model { data | exerciseInput = TimeInput.updateInput data.exerciseInput newVal }, Cmd.none )

        UpdateFocus Exercise isFocused ->
            Config model { data | exerciseInput = TimeInput.updateFocus data.exerciseInput isFocused }
                |> saveToLocalStorage

        UpdateInput Break newVal ->
            ( Config model { data | breakInput = TimeInput.updateInput data.breakInput newVal }, Cmd.none )

        UpdateFocus Break isFocused ->
            Config model { data | breakInput = TimeInput.updateFocus data.breakInput isFocused }
                |> saveToLocalStorage

        UpdateInput SetBreak newVal ->
            ( Config model { data | setBreakInput = TimeInput.updateInput data.setBreakInput newVal }, Cmd.none )

        UpdateFocus SetBreak isFocused ->
            Config model { data | setBreakInput = TimeInput.updateFocus data.setBreakInput isFocused }
                |> saveToLocalStorage

        UpdateInput Countdown newVal ->
            ( Config model { data | countdownInput = TimeInput.updateInput data.countdownInput newVal }, Cmd.none )

        UpdateFocus Countdown isFocused ->
            Config model { data | countdownInput = TimeInput.updateFocus data.countdownInput isFocused }
                |> saveToLocalStorage

        ToggleCountdown bool ->
            Config model { data | countdown = bool }
                |> saveToLocalStorage

        NewElement setPos ->
            Config model
                { data
                    | sets =
                        Dict.update
                            setPos
                            (Maybe.map Set.newExercise)
                            data.sets
                }
                |> saveToLocalStorage

        DeleteElement setPos elemPos ->
            Config model
                { data
                    | sets =
                        Dict.update
                            setPos
                            (Maybe.map <| (Set.deleteExercise elemPos >> Set.sanitizeExercises))
                            data.sets
                }
                |> saveToLocalStorage

        NewSetRepeat setPos repeat ->
            Config model
                { data
                    | sets =
                        Dict.update
                            setPos
                            (Maybe.map <| Set.updateRepeat repeat)
                            data.sets
                }
                |> saveToLocalStorage

        DeleteSet setPos ->
            Config model (Data.sanitizeSets { data | sets = Dict.remove setPos data.sets })
                |> saveToLocalStorage

        AddSet ->
            let
                newN =
                    data.setCounter + 1
            in
            Config model
                { data
                    | sets = Dict.insert newN (Set.init newN) data.sets
                    , setCounter = newN
                }
                |> saveToLocalStorage

        CopySet setPos ->
            case Dict.get setPos data.sets of
                Nothing ->
                    ( Config model data, Cmd.none )

                Just setToCopy ->
                    let
                        -- creating a new dict, shifting the keys of the element to make room for the duplicated element
                        newSets =
                            Dict.toList data.sets
                                |> List.map
                                    (\( n, set ) ->
                                        if n > setPos then
                                            ( n + 1, set )

                                        else
                                            ( n, set )
                                    )
                                |> Dict.fromList
                    in
                    Config model
                        { data
                            | sets = Dict.insert (setPos + 1) (Set.updatePosition (setPos + 1) setToCopy) newSets
                            , setCounter = Dict.size data.sets + 1
                        }
                        |> saveToLocalStorage

        ToggleSetExpand setPos ->
            ( Config model { data | sets = Dict.update setPos (Maybe.map Set.toggleExpand) data.sets }, Cmd.none )

        UpdateSetName setPos newName ->
            Config model { data | sets = Dict.update setPos (Maybe.map <| Set.updateName newName) data.sets }
                |> saveToLocalStorage

        UpdateExerciseName setPos exercisePos newName ->
            Config model { data | sets = Dict.update setPos (Maybe.map <| Set.updateExerciseName exercisePos newName) data.sets }
                |> saveToLocalStorage

        ToLocalStorage ->
            ( Config { model | saving = True } data, Ports.storeConfig (Data.encode data) )

        StoreConfigSuccess ->
            ( Config { model | saving = False } data, Cmd.none )



-- helpers


saveToLocalStorage : Config -> ( Config, Cmd Msg )
saveToLocalStorage config =
    update ToLocalStorage config



---- SUBSCRIPTIONS ----


subscriptions : Config -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize NewWindowSize
        , Ports.storeConfigSuccess <| always StoreConfigSuccess
        ]
