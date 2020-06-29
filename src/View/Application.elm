module View.Application exposing
    ( Application
    , Msg
    , endWorkout
    , exercising
    , init
    , subscriptions
    , update
    , updateData
    , view
    )

import Browser.Events
import Colours
import Data.Application as Data exposing (Data)
import Data.Config
import Data.Flags as Flags exposing (Flags)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icon
import Keyboard exposing (Key)
import List.Nonempty exposing (Nonempty(..))
import Ports
import Time
import Util



---- TYPE ----


type Application
    = Application Model Data



-- other important data


type alias Model =
    { keys : List Key -- keys pressed down
    , smhSrc : String
    , device : Element.Device
    }



-- should only be called once


init : Data.Config.Data -> Flags -> Application
init data flags =
    Data.fromConfig data
        |> Application
            { keys = []
            , smhSrc = flags.smhSrc
            , device = Element.classifyDevice flags.windowSize
            }



-- every time the user switches to the application page


updateData : Data.Config.Data -> Application -> Application
updateData data (Application model _) =
    Data.fromConfig data
        |> Application model



-- Main.elm uses this to know whether or not they need to show the settings page at the bottom


exercising : Application -> Bool
exercising (Application _ data) =
    case data.state of
        Data.InProgress _ _ ->
            True

        _ ->
            False



-- Main.elm also uses this to end the workout
-- changing state so it won't keep ticking when we're on the settings page
-- I just arbitratily put the state as Finished - honestly, anything that's not Data.InProgress will work


endWorkout : Application -> Application
endWorkout (Application model _) =
    Application model
        { playing = False
        , state = Data.Finished
        }



---- VIEW ----


view : Application -> Element Msg
view (Application model data) =
    case data.state of
        Data.Starting blocks ->
            let
                title =
                    Element.paragraph
                        [ Font.size 50
                        , Font.center
                        , Font.color Colours.sunset
                        ]
                        [ Element.text "Ready?"
                        ]

                startExerciseButton =
                    Util.viewIcon
                        { icon = Icon.play
                        , color = Colours.sunset
                        , size = 75
                        , msg = Just <| StartExercise blocks
                        , withBorder = True
                        }

                subTitle =
                    Element.paragraph
                        [ Font.color Colours.sunset
                        , Font.size 20
                        , Font.center
                        , Font.light
                        ]
                        [ Element.el [ Font.bold ] <| Element.text "Note:"
                        , Element.text " if you press play, you "
                        , Element.el [ Font.bold ] <| Element.text "cannot"
                        , Element.text " go back to the settings page without resetting your workout!"
                        ]
            in
            -- I have to use Util.centerOverlay to ensure that both the upper and lower blocks are the SAME height
            if Util.isVerticalPhone model.device then
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.padding 16
                    ]
                    [ Element.el
                        [ Element.height Element.fill
                        , Element.width Element.fill
                        , Util.centerOverlay title
                        ]
                        Element.none
                    , Element.el [ Element.centerX ] startExerciseButton
                    , Element.el
                        [ Element.height Element.fill
                        , Element.width Element.fill
                        , Util.centerOverlay subTitle
                        ]
                        Element.none
                    ]

            else
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.below <|
                            Element.el
                                [ Element.centerX
                                , Element.alignBottom
                                , Element.moveUp 60
                                , Element.padding 4
                                ]
                                startExerciseButton
                        , Util.centerOverlay title
                        ]
                        Element.none
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Util.centerOverlay subTitle
                        ]
                        Element.none
                    ]

        Data.InProgress totalBlockCount blocks ->
            let
                playPauseIcon =
                    if data.playing then
                        Icon.pause

                    else
                        Icon.play
            in
            -- TODO: REFACTOR TO REDUCE THE AMOUNT OF SIMILAR CODE
            if Util.isVerticalPhone model.device then
                let
                    bigFont size color label =
                        Element.paragraph
                            [ Element.centerX
                            , Font.size size
                            , Font.center
                            , Font.color color
                            , Font.light
                            ]
                            [ Element.text label ]

                    ( currBlockelem, themeColor, remainingTime ) =
                        case List.Nonempty.head blocks of
                            Data.CountDown secsLeft _ ->
                                ( bigFont 32 Colours.sky "Countdown", Colours.sky, secsLeft )

                            Data.ExerciseBreak secsLeft _ ->
                                ( bigFont 32 Colours.grass "Break Between Exercises", Colours.grass, secsLeft )

                            Data.SetBreak secsLeft _ ->
                                ( bigFont 32 Colours.grass "Break Between Sets", Colours.grass, secsLeft )

                            Data.Exercise { setName, name, duration, secsLeft } ->
                                ( Element.column
                                    [ Element.centerX
                                    , Element.spacing 4
                                    , Font.center
                                    , Font.color Colours.sky
                                    ]
                                    [ bigFont 32 Colours.sunflower setName
                                    , bigFont 48 Colours.sunset name
                                    ]
                                , Colours.sunset
                                , secsLeft
                                )

                    nextupString =
                        case List.head <| List.Nonempty.tail blocks of
                            Just (Data.ExerciseBreak _ _) ->
                                "Break"

                            Just (Data.SetBreak _ _) ->
                                "Break"

                            Just (Data.Exercise d) ->
                                d.name

                            _ ->
                                "Workout Completion"

                    viewRemainingTime =
                        Element.el
                            [ Font.size 72
                            , Font.color themeColor
                            , Font.bold
                            ]
                        <|
                            Element.text (String.fromInt remainingTime)

                    bottomElems =
                        Element.column
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Element.spaceEvenly
                            ]
                            [ Element.el
                                [ Element.width Element.fill
                                , Element.height (Element.px 1)
                                ]
                                Element.none
                            , -- play/pause toggle
                              Input.button
                                [ Element.width (Element.maximum 150 Element.fill)
                                , Element.height Element.shrink
                                , Element.centerX
                                , Border.rounded 15
                                , Background.color Colours.sky
                                ]
                                { onPress = Just TogglePlay
                                , label =
                                    Element.el
                                        [ Element.centerX
                                        , Element.padding 8
                                        ]
                                    <|
                                        Util.viewIcon
                                            { icon = playPauseIcon
                                            , color = Colours.white
                                            , size = 30
                                            , msg = Nothing
                                            , withBorder = False
                                            }
                                }

                            -- next up
                            , Element.paragraph
                                [ Element.height Element.shrink
                                , Font.center
                                , Font.size 20
                                , Font.light
                                , Font.color Colours.sky
                                ]
                                [ Element.text "Next up: "
                                , Element.text nextupString
                                ]

                            -- instagram-like timeline thing
                            , let
                                currExerciseNum =
                                    List.Nonempty.length blocks

                                viewPebble n =
                                    let
                                        ( pebbleHeight, pebbleColour ) =
                                            if n == currExerciseNum then
                                                -- this pebble represents the current exercise
                                                ( 9, themeColor )

                                            else if n > currExerciseNum then
                                                ( 5, Colours.lightGray )

                                            else
                                                ( 5, Colours.withAlpha 0.6 Colours.sky )
                                    in
                                    Element.el
                                        [ Element.width Element.fill
                                        , Element.centerY
                                        , Element.height (Element.px pebbleHeight)
                                        , Border.rounded 3
                                        , Background.color pebbleColour
                                        ]
                                        Element.none
                              in
                              List.range 1 totalBlockCount
                                |> List.reverse
                                |> List.map viewPebble
                                |> Element.row
                                    [ Element.spacing 2
                                    , Element.width Element.fill
                                    ]
                            , Element.el
                                [ Element.width Element.fill
                                , Element.height (Element.px 1)
                                ]
                                Element.none
                            ]
                in
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.padding 16
                    ]
                    [ -- current exercise
                      Element.el
                        [ Element.width Element.fill
                        , Element.height <| Element.fillPortion 2
                        , Util.centerOverlay currBlockelem
                        ]
                        Element.none

                    -- next exercise
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height <| Element.fillPortion 3
                        , Util.centerOverlay viewRemainingTime
                        ]
                        Element.none

                    -- bottom pause bar and dots
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height <| Element.fillPortion 2
                        ]
                        bottomElems
                    ]

            else
                let
                    bigFont size color label =
                        Element.el
                            [ Element.centerX
                            , Font.size size
                            , Font.center
                            , Font.color color
                            , Font.light
                            ]
                        <|
                            Element.text label

                    timerText secsLeft color =
                        Element.el
                            [ Element.centerX
                            , Font.color color
                            , Font.size 125
                            ]
                        <|
                            Element.text <|
                                String.fromInt secsLeft

                    timerBar secsLeft total color =
                        Element.row
                            [ Element.width Element.fill ]
                            [ Element.el
                                [ Element.width <| Element.fillPortion secsLeft
                                , Element.height (Element.px 5)
                                , Background.color color
                                ]
                                Element.none
                            , Element.el
                                [ Element.width <| Element.fillPortion (total - secsLeft) ]
                                Element.none
                            ]

                    dataGroup =
                        case List.Nonempty.head blocks of
                            Data.CountDown secsLeft total ->
                                { upperElem = bigFont 32 Colours.sky "Countdown"
                                , timerText = timerText secsLeft Colours.sky
                                , timerBar = timerBar secsLeft total Colours.sky
                                , theme = Colours.sky
                                }

                            Data.ExerciseBreak secsLeft total ->
                                { upperElem = bigFont 32 Colours.grass "Break Between Exercise"
                                , timerText = timerText secsLeft Colours.grass
                                , timerBar = timerBar secsLeft total Colours.grass
                                , theme = Colours.grass
                                }

                            Data.SetBreak secsLeft total ->
                                { upperElem = bigFont 32 Colours.grass "Break Between Sets"
                                , timerText = timerText secsLeft Colours.grass
                                , timerBar = timerBar secsLeft total Colours.grass
                                , theme = Colours.grass
                                }

                            Data.Exercise { setName, name, duration, secsLeft } ->
                                { upperElem =
                                    Element.column
                                        [ Element.centerX
                                        , Element.spacing 16
                                        , Font.size 32
                                        , Font.center
                                        , Font.color Colours.sky
                                        ]
                                        [ bigFont 32 Colours.sunflower setName
                                        , bigFont 48 Colours.sunset name
                                        ]
                                , timerText = timerText secsLeft Colours.sunset
                                , timerBar = timerBar secsLeft duration Colours.sunset
                                , theme = Colours.sunset
                                }

                    nextupString =
                        case List.head <| List.Nonempty.tail blocks of
                            Just (Data.ExerciseBreak _ _) ->
                                "Break"

                            Just (Data.SetBreak _ _) ->
                                "Break"

                            Just (Data.Exercise d) ->
                                d.name

                            _ ->
                                "Workout Completion"

                    nextup =
                        Element.paragraph
                            [ Element.centerX
                            , Font.size 32
                            , Font.color Colours.sky
                            , Font.light
                            ]
                            [ Element.text "Next up: "
                            , Element.text nextupString
                            ]
                in
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ -- Toggle button and all things above the button
                      Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.below
                            (Util.viewIcon
                                { icon = playPauseIcon
                                , color = dataGroup.theme
                                , size = 75
                                , msg = Just TogglePlay
                                , withBorder = True
                                }
                                |> Element.el
                                    [ Element.centerX
                                    , Element.moveUp 56
                                    , Element.padding 4
                                    , Background.color Colours.white
                                    ]
                                |> Element.el
                                    [ Element.width Element.fill
                                    , Element.behindContent <|
                                        Element.el
                                            [ Element.width Element.fill ]
                                            dataGroup.timerBar
                                    ]
                            )
                        , Element.inFront <|
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                                dataGroup.upperElem
                        ]
                        Element.none

                    -- all things below the button
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.inFront <|
                            Element.column
                                [ Element.centerX
                                , Element.centerY
                                , Element.spacing 16
                                ]
                                [ dataGroup.timerText
                                , nextup
                                ]
                        ]
                        Element.none
                    ]

        Data.Finished ->
            Element.column
                [ Element.width Element.fill
                , Element.centerY
                , Element.spacing 32
                ]
                [ Util.viewIcon
                    { icon = Icon.star
                    , color = Colours.sunflower
                    , size = 100
                    , msg = Nothing
                    , withBorder = False
                    }
                    |> Element.el
                        [ Element.centerX ]
                , Element.paragraph
                    [ Font.color Colours.sunflower
                    , Font.center
                    , Font.light
                    , Font.size 50
                    ]
                    [ Element.text "WOOHOO!" ]
                , Element.paragraph
                    [ Font.color Colours.sunflower
                    , Font.center
                    , Font.light
                    ]
                    [ Element.text "Congratulations! You finished!" ]
                ]

        Data.NeverStarted ->
            Element.column
                [ Element.width Element.fill
                , Element.centerY
                , Element.spacing 32
                ]
                [ Element.image
                    [ Element.width (Element.px 125)
                    , Element.centerX
                    ]
                    { src = model.smhSrc
                    , description = "Sokka is disappointed in your workout"
                    }
                , Element.paragraph
                    [ Font.color Colours.sunset
                    , Font.center
                    , Font.light
                    , Font.size 50
                    ]
                    [ Element.text "Disappointed." ]
                , Element.textColumn
                    [ Element.centerX
                    , Element.spacing 4
                    , Element.width Element.fill
                    , Font.light
                    , Font.center
                    , Font.color Colours.sunset
                    ]
                    [ Element.paragraph
                        []
                        [ Element.text "You didn't put anything in your workout!" ]
                    , Element.paragraph
                        []
                        [ Element.text "Go to the settings and try again." ]
                    ]
                ]



---- UPDATE ----


type Msg
    = NewWindowSize Int Int
    | StartExercise (Nonempty Data.TimeBlock)
    | NextSecond
    | TogglePlay
    | KeyMsg Keyboard.Msg -- so we can react upon the space key press


update : Msg -> Application -> ( Application, Cmd Msg )
update msg (Application model data) =
    case msg of
        NewWindowSize width height ->
            ( Application { model | device = Element.classifyDevice <| Flags.WindowSize width height } data, Cmd.none )

        StartExercise blocks ->
            ( Application model
                { data
                    | state = Data.InProgress (List.Nonempty.length blocks) blocks
                    , playing = True
                }
            , Ports.playWhistle ()
            )

        NextSecond ->
            case data.state of
                Data.InProgress totalBlockCount (Nonempty block tl) ->
                    case Data.decreaseTimeBlock block of
                        Nothing ->
                            case tl of
                                -- no more exercises
                                [] ->
                                    ( Application model { data | state = Data.Finished }, Ports.playTada () )

                                x :: xs ->
                                    ( Application model { data | state = Data.InProgress totalBlockCount <| Nonempty x xs }, Ports.playWhistle () )

                        Just newBlock ->
                            let
                                cmd =
                                    if Data.timeLeft newBlock <= 3 then
                                        Ports.playTick ()

                                    else
                                        Cmd.none
                            in
                            ( Application model { data | state = Data.InProgress totalBlockCount <| Nonempty newBlock tl }, cmd )

                _ ->
                    -- ignore
                    ( Application model data, Cmd.none )

        TogglePlay ->
            ( Application model { data | playing = not data.playing }
            , if data.playing then
                Cmd.none

              else
                Ports.playWhistle ()
            )

        KeyMsg keyMsg ->
            let
                newKeys =
                    Keyboard.update keyMsg model.keys

                newModel =
                    { model | keys = newKeys }
            in
            if newKeys == [ Keyboard.Spacebar ] then
                update TogglePlay (Application newModel data)

            else
                ( Application model data, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Application -> Sub Msg
subscriptions (Application _ data) =
    let
        tickSub =
            case data.state of
                Data.InProgress _ _ ->
                    if data.playing then
                        Time.every 1000 (always NextSecond)

                    else
                        Sub.none

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        , Browser.Events.onResize NewWindowSize
        , tickSub
        ]
