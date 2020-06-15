module Main exposing (main)

import Browser
import Colours
import Data.Duration as Duration exposing (Duration)
import Data.Flags exposing (Flags, WindowSize)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icon
import Html exposing (Html)
import Modules.Exercise as Exercise exposing (Exercise)
import Modules.Set as Set exposing (Set)
import Modules.TimeInput as TimeInput exposing (TimeInput)
import Task
import Time
import Util



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { time : Time.Posix
    , zone : Time.Zone
    , windowSize : WindowSize
    , state : State

    -- internal configuration data
    , config : Config

    -- internal config for app data (e.g. play/pause) but also the exercises
    , appData : AppData
    }


type State
    = Settings
    | Application


type alias Config =
    { exerciseInput : TimeInput
    , breakInput : TimeInput
    , setBreakInput : TimeInput
    , countdown : Bool
    , countdownInput : TimeInput
    , set : Dict Int Set
    , setCounter : Int
    }


type alias AppData =
    { exercises : List ( String, List ( String, Duration ) )
    , playing : Bool
    , countdown : Maybe Duration
    }



-- either the exercise, break, countdown or setBreak


type Input
    = Exercise
    | Break
    | SetBreak
    | Countdown


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        setDict =
            List.map2 Tuple.pair (List.range 1 2) (List.map Set.init <| List.range 1 2)
                |> Dict.fromList
    in
    ( { time = Time.millisToPosix flags.posix
      , zone = Time.utc
      , windowSize = flags.windowSize
      , state = Settings
      , config =
            { exerciseInput = TimeInput.init 60
            , breakInput = TimeInput.init 15
            , setBreakInput = TimeInput.init 60
            , countdown = False
            , countdownInput = TimeInput.init 5
            , set = setDict
            , setCounter = 2
            }
      , appData =
            { playing = False
            , exercises = []
            , countdown = Just (Duration.init 5)
            }
      }
    , Task.perform AdjustTimeZone Time.here
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        navButton =
            case model.state of
                Settings ->
                    Util.viewIcon
                        { icon = Icon.zap
                        , color = Colours.sunset
                        , size = 50
                        , msg = Just SaveSettings
                        }

                Application ->
                    Util.viewIcon
                        { icon = Icon.sliders
                        , color = Colours.sky
                        , size = 50
                        , msg = Just ToSettings
                        }
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 16
        , Element.spacing 32
        , Element.inFront <|
            Element.el
                [ Element.alignLeft
                , Element.moveDown (toFloat model.windowSize.height - 150)
                , Element.padding 32
                ]
                navButton
        ]
        [ Element.el
            [ Element.centerX ]
          <|
            clock model.time model.zone
        , case model.state of
            Settings ->
                settings model.config

            Application ->
                application model.appData
        ]
        |> Element.layout
            [ Font.family
                [ Font.typeface "Lato" ]
            ]


clock : Time.Posix -> Time.Zone -> Element Msg
clock time zone =
    let
        hour =
            String.fromInt (Time.toHour zone time)

        minute =
            String.pad 2 '0' <| String.fromInt (Time.toMinute zone time)
    in
    Element.paragraph
        [ Font.center
        , Font.size 48
        , Font.light
        ]
        [ Element.text hour
        , Element.text ":"
        , Element.text minute
        ]


application : AppData -> Element Msg
application appData =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 32
        , Element.spacing 48
        ]
        [ Util.viewIcon
            { icon = Icon.zap
            , color = Colours.sunset
            , size = 50
            , msg = Nothing
            }
            |> Element.el
                [ Element.centerX
                ]
        , if appData.playing then
            Element.text "lol"

          else
            Element.column
                [ Element.width Element.fill
                , Element.centerY
                ]
                [ Util.viewIcon
                    { icon = Icon.play
                    , color = Colours.sunset
                    , size = 100
                    , msg = Nothing
                    }
                    |> Element.el
                        [ Element.centerX
                        ]
                , Element.paragraph
                    [ Font.color Colours.sunset
                    , Font.center
                    , Font.light
                    , Font.size 50
                    ]
                    [ Element.text "Ready?" ]
                ]
        ]


settings : Config -> Element Msg
settings config =
    Element.column
        [ Element.width (Element.fill |> Element.maximum 1000)
        , Element.centerX
        , Element.height Element.fill
        , Element.padding 32
        , Element.spacing 48
        ]
        [ Util.viewIcon
            { icon = Icon.sliders
            , color = Colours.sky
            , size = 50
            , msg = Nothing
            }
            |> Element.el
                [ Element.centerX
                ]

        -- actual settings stuff
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ config.exerciseInput
                |> TimeInput.view
                    { updateInput = UpdateInput Exercise
                    , updateFocus = UpdateFocus Exercise
                    , displayText = Just "Exercise Duration:"
                    }
            , config.breakInput
                |> TimeInput.view
                    { updateInput = UpdateInput Break
                    , updateFocus = UpdateFocus Break
                    , displayText = Just "Break Between Exercises:"
                    }
            , config.setBreakInput
                |> TimeInput.view
                    { updateInput = UpdateInput SetBreak
                    , updateFocus = UpdateFocus SetBreak
                    , displayText = Just "Break Between Sets:"
                    }

            -- countdown
            , Element.row
                [ Element.spacing 8
                , Element.centerX
                ]
                [ Input.checkbox
                    [ Font.light
                    , Element.padding 2
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
                    , checked = config.countdown
                    , label = Input.labelLeft [ Element.padding 8, Element.centerY ] <| Element.text "Countdown:"
                    }
                    |> Element.el
                        [ Element.centerX ]
                , if config.countdown then
                    config.countdownInput
                        |> TimeInput.view
                            { updateInput = UpdateInput Countdown
                            , updateFocus = UpdateFocus Countdown
                            , displayText = Nothing
                            }

                  else
                    Element.none
                ]
            ]

        -- set stuff
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 18
            ]
            [ Dict.toList
                config.set
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
                            , exerciseDuration = TimeInput.getDuration config.exerciseInput
                            , breakDuration = TimeInput.getDuration config.breakInput
                            }
                            set
                    )
                |> List.intersperse (Exercise.breakView <| TimeInput.getDuration config.setBreakInput)
                |> Element.column
                    [ Element.spacing 32
                    , Element.width Element.fill
                    ]
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

        -- save settings
        , Util.viewIcon
            { icon = Icon.check
            , color = Colours.grass
            , size = 50
            , msg = Just SaveSettings
            }
            |> Element.el
                [ Element.centerX
                ]
        ]



---- UPDATE ----


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | UpdateInput Input String
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
    | SaveSettings
    | ToSettings
    | NoOp



-- really should use lenses


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        -- exercise input msgs
        UpdateInput Exercise newVal ->
            updateExercise model TimeInput.updateInput newVal

        UpdateFocus Exercise isFocused ->
            updateExercise model TimeInput.updateFocus isFocused

        -- break input msgs
        UpdateInput Break newVal ->
            updateBreak model TimeInput.updateInput newVal

        UpdateFocus Break isFocused ->
            updateBreak model TimeInput.updateFocus isFocused

        -- break input msgs
        UpdateInput SetBreak newVal ->
            updateSetBreak model TimeInput.updateInput newVal

        UpdateFocus SetBreak isFocused ->
            updateSetBreak model TimeInput.updateFocus isFocused

        -- countdown input msgs
        UpdateInput Countdown newVal ->
            updateCountdown model TimeInput.updateInput newVal

        UpdateFocus Countdown isFocused ->
            updateCountdown model TimeInput.updateFocus isFocused

        ToggleCountdown bool ->
            let
                config =
                    model.config

                newConfig =
                    { config
                        | countdown = bool
                    }
            in
            ( { model | config = newConfig }, Cmd.none )

        NewElement setPos ->
            let
                config =
                    model.config

                newSet =
                    Dict.update
                        setPos
                        (Maybe.map Set.newExercise)
                        config.set

                newConfig =
                    { config | set = newSet }
            in
            ( { model | config = newConfig }
            , Cmd.none
            )

        DeleteElement setPos elemPos ->
            let
                config =
                    model.config

                newSet =
                    Dict.update
                        setPos
                        (Maybe.map <| Set.deleteExercise elemPos)
                        config.set

                newConfig =
                    { config | set = newSet }
            in
            ( { model | config = newConfig }, Cmd.none )

        NewSetRepeat setPos repeat ->
            let
                config =
                    model.config

                newSet =
                    Dict.update
                        setPos
                        (Maybe.map <| Set.updateRepeat repeat)
                        config.set

                newConfig =
                    { config | set = newSet }
            in
            ( { model | config = newConfig }, Cmd.none )

        DeleteSet setPos ->
            let
                config =
                    model.config

                newConfig =
                    { config | set = Dict.remove setPos model.config.set }
            in
            ( { model | config = newConfig }, Cmd.none )

        AddSet ->
            let
                config =
                    model.config

                newN =
                    config.setCounter + 1

                newConfig =
                    { config
                        | set = Dict.insert newN (Set.init newN) config.set
                        , setCounter = newN
                    }
            in
            ( { model | config = newConfig }, Cmd.none )

        ToggleSetExpand setPos ->
            let
                config =
                    model.config

                newSet =
                    Dict.update setPos (Maybe.map Set.toggleExpand) config.set

                newConfig =
                    { config | set = newSet }
            in
            ( { model | config = newConfig }, Cmd.none )

        UpdateSetName setPos newName ->
            let
                config =
                    model.config

                newSet =
                    Dict.update setPos (Maybe.map <| Set.updateName newName) config.set

                newConfig =
                    { config | set = newSet }
            in
            ( { model | config = newConfig }, Cmd.none )

        UpdateExerciseName setPos exercisePos newName ->
            let
                config =
                    model.config

                newSet =
                    Dict.update setPos (Maybe.map <| Set.updateExerciseName exercisePos newName) config.set

                newConfig =
                    { config | set = newSet }
            in
            ( { model | config = newConfig }, Cmd.none )

        SaveSettings ->
            let
                appData =
                    model.appData

                newAppData =
                    { appData
                        | exercises =
                            model.config.set
                                |> Dict.map (\_ -> Set.getEssentials (TimeInput.getDuration model.config.exerciseInput))
                                |> Dict.toList
                                |> List.map Tuple.second
                        , countdown =
                            if model.config.countdown then
                                Just <| TimeInput.getDuration model.config.countdownInput

                            else
                                Nothing
                    }
            in
            ( { model
                | state = Application
                , appData = newAppData
              }
            , Cmd.none
            )

        ToSettings ->
            ( { model | state = Settings }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateExercise : Model -> (TimeInput -> a -> TimeInput) -> a -> ( Model, Cmd Msg )
updateExercise model f a =
    let
        config =
            model.config

        newConfig =
            { config | exerciseInput = f config.exerciseInput a }
    in
    ( { model | config = newConfig }, Cmd.none )


updateBreak : Model -> (TimeInput -> a -> TimeInput) -> a -> ( Model, Cmd Msg )
updateBreak model f a =
    let
        config =
            model.config

        newConfig =
            { config | breakInput = f config.breakInput a }
    in
    ( { model | config = newConfig }, Cmd.none )


updateSetBreak : Model -> (TimeInput -> a -> TimeInput) -> a -> ( Model, Cmd Msg )
updateSetBreak model f a =
    let
        config =
            model.config

        newConfig =
            { config | setBreakInput = f config.setBreakInput a }
    in
    ( { model | config = newConfig }, Cmd.none )


updateCountdown : Model -> (TimeInput -> a -> TimeInput) -> a -> ( Model, Cmd Msg )
updateCountdown model f a =
    let
        config =
            model.config

        newConfig =
            { config | countdownInput = f config.countdownInput a }
    in
    ( { model | config = newConfig }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
