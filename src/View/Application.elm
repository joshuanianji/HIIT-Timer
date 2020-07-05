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
    , screenDimensions : Flags.WindowSize
    , device : Element.Device
    }



-- should only be called once


init : Data.Config.Data -> Flags -> Application
init data flags =
    Data.fromConfig data
        |> Application
            { keys = []
            , smhSrc = flags.images.smhSrc
            , screenDimensions = flags.windowSize
            , device = Element.classifyDevice flags.windowSize
            }



-- every time the user switches to the application page from config, we only update config data.


updateData : Data.Config.Data -> Application -> Application
updateData data (Application model _) =
    Data.fromConfig data
        |> Application model



-- Main.elm uses this to know whether or not they need to show the settings page at the bottom


exercising : Application -> Bool
exercising (Application _ data) =
    case data.state of
        Data.InProgress _ ->
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
        Data.Starting workoutData ->
            viewStarting workoutData model

        Data.InProgress workoutData ->
            viewInProgress workoutData model data

        Data.Finished ->
            viewFinished

        Data.NeverStarted ->
            -- the smh screen
            viewNeverStarted model


viewStarting : Data.WorkoutData -> Model -> Element Msg
viewStarting workoutData model =
    let
        title n =
            Element.paragraph
                [ Font.size n
                , Font.center
                , Font.color Colours.sunset
                ]
                [ Element.text "Ready?" ]

        info n =
            Element.paragraph
                [ Font.light
                , Font.size n
                ]
                []

        startExerciseButton =
            Util.viewIcon
                { icon = Icon.play
                , color = Colours.sunset
                , size = 75
                , msg = Just <| StartExercise workoutData
                , withBorder = True
                }

        subTitle n displayPortrait =
            let
                heading =
                    if displayPortrait then
                        Element.el
                            [ Element.centerX
                            , Font.bold
                            , Font.size 32
                            ]
                        <|
                            Element.text "Note:"

                    else
                        Element.el [ Font.bold ] <| Element.text "Note:"

                message =
                    Element.paragraph
                        []
                        [ Element.text " if you press play, you "
                        , Element.el [ Font.bold ] <| Element.text "cannot"
                        , Element.text " go back to the settings page without resetting your workout!"
                        ]

                attrs =
                    [ Font.color Colours.sunset
                    , Font.size n
                    , Font.center
                    , Font.light
                    ]
            in
            if displayPortrait then
                Element.column (Element.spacing 8 :: attrs) [ heading, message ]

            else
                Element.paragraph attrs [ heading, message ]
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
                , Util.centerOverlay <| title 50
                ]
                Element.none
            , Element.el [ Element.centerX ] startExerciseButton
            , Element.el
                [ Element.height Element.fill
                , Element.width Element.fill
                , Util.centerOverlay <| subTitle 20 False
                ]
                Element.none
            ]

    else
        let
            data =
                case model.device.orientation of
                    -- the icons are in a column, but the Ready and the Note are in the left and ride sides respectively
                    Element.Landscape ->
                        { parentElem = Element.row
                        , titleSize = 75
                        , subTitleElem = subTitle 25 True
                        , subTitleSurround = Util.surround 1 2 1
                        , startExerciseButtonParent =
                            Element.el
                                [ Element.centerY
                                , Element.width Element.shrink
                                ]
                        }

                    Element.Portrait ->
                        { parentElem = Element.column
                        , titleSize = 50
                        , subTitleElem = subTitle 20 False
                        , subTitleSurround = identity
                        , startExerciseButtonParent =
                            Element.el
                                [ Element.centerX
                                , Element.padding 4
                                ]
                        }
        in
        data.parentElem
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Util.centerOverlay <| title data.titleSize
                ]
                Element.none
            , data.startExerciseButtonParent
                startExerciseButton
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Util.centerOverlay data.subTitleElem
                ]
                Element.none
                |> data.subTitleSurround
            ]


viewInProgress : Data.WorkoutData -> Model -> Data -> Element Msg
viewInProgress workoutData model data =
    let
        playPauseIcon =
            if data.playing then
                Icon.pause

            else
                Icon.play

        pebbles currBlockColour { secsLeft, totalTime } =
            let
                currExerciseNum =
                    List.Nonempty.length workoutData.blocksLeft

                viewPebble n =
                    let
                        -- if the user is not on a phone, we'll make it have the little timer bar for the current pebble
                        specialPebble height colour =
                            Element.row
                                [ Element.width Element.fill
                                , Element.centerY
                                , Element.height (Element.px height)
                                , Border.rounded 3
                                , Background.color (Colours.withAlpha 0.6 colour)
                                ]
                                [ Element.el
                                    [ Element.width <| Element.fillPortion (totalTime - secsLeft) ]
                                    Element.none
                                , Element.el
                                    [ Element.width <| Element.fillPortion secsLeft
                                    , Element.height (Element.px height)
                                    , Background.color colour
                                    , Border.rounded 3
                                    ]
                                    Element.none
                                ]

                        ( pebbleHeight, pebbleColour, useSpecial ) =
                            if n == currExerciseNum then
                                -- this pebble represents the current exercise
                                ( 9, currBlockColour, True )

                            else if n > currExerciseNum then
                                ( 5, Colours.lightGray, False )

                            else
                                ( 5, Colours.withAlpha 0.6 Colours.sky, False )
                    in
                    if useSpecial then
                        specialPebble pebbleHeight pebbleColour

                    else
                        Element.el
                            [ Element.width Element.fill
                            , Element.centerY
                            , Element.height (Element.px pebbleHeight)
                            , Border.rounded 3
                            , Background.color pebbleColour
                            ]
                            Element.none
            in
            List.range 1 workoutData.info.totalTimeblocks
                |> List.reverse
                |> List.map viewPebble
                |> Element.row
                    [ Element.spacing 2
                    , Element.width Element.fill
                    ]

        viewPhone =
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

                dataGroup =
                    case List.Nonempty.head workoutData.blocksLeft of
                        Data.CountDown secsLeft totalTime ->
                            { currBlockElem = bigFont 32 Colours.sky "Countdown"
                            , themeColour = Colours.sky
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        Data.ExerciseBreak secsLeft totalTime ->
                            { currBlockElem = bigFont 32 Colours.grass "Break Between Exercises"
                            , themeColour = Colours.grass
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        Data.SetBreak secsLeft totalTime ->
                            { currBlockElem = bigFont 32 Colours.grass "Break Between Sets"
                            , themeColour = Colours.grass
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        Data.Exercise { setName, name, duration, secsLeft } ->
                            { currBlockElem =
                                Element.column
                                    [ Element.centerX
                                    , Element.spacing 4
                                    , Font.center
                                    , Font.color Colours.sky
                                    ]
                                    [ bigFont 32 Colours.sunflower setName
                                    , bigFont 48 Colours.sunset name
                                    ]
                            , themeColour = Colours.sunset
                            , secsLeft = secsLeft
                            , totalTime = duration
                            }

                nextupString =
                    case List.head <| List.Nonempty.tail workoutData.blocksLeft of
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
                        [ Font.size <| model.screenDimensions.height // 8
                        , Font.color dataGroup.themeColour
                        , Font.bold
                        ]
                    <|
                        Element.text (String.fromInt dataGroup.secsLeft)

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
                        , pebbles dataGroup.themeColour
                            { secsLeft = dataGroup.secsLeft
                            , totalTime = dataGroup.totalTime
                            }
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
                    , Util.centerOverlay dataGroup.currBlockElem
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

        viewDesktop =
            let
                bigFont size color label =
                    Element.el
                        [ Element.centerX
                        , Font.size size
                        , Font.center
                        , Font.color color
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

                dataGroup =
                    case List.Nonempty.head workoutData.blocksLeft of
                        Data.CountDown secsLeft totalTime ->
                            { currExercise = bigFont 54 Colours.sky "Countdown"
                            , timerText = timerText secsLeft Colours.sky
                            , themeColour = Colours.sky
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        Data.ExerciseBreak secsLeft totalTime ->
                            { currExercise = bigFont 54 Colours.grass "Break Between Exercise"
                            , timerText = timerText secsLeft Colours.grass
                            , themeColour = Colours.grass
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        Data.SetBreak secsLeft totalTime ->
                            { currExercise = bigFont 54 Colours.grass "Break Between Sets"
                            , timerText = timerText secsLeft Colours.grass
                            , themeColour = Colours.grass
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        Data.Exercise { setName, name, duration, secsLeft } ->
                            { currExercise =
                                Element.column
                                    [ Element.centerX
                                    , Element.spacing 16
                                    , Font.center
                                    , Font.color Colours.sky
                                    ]
                                    [ bigFont 32 Colours.sunflower setName
                                    , bigFont 54 Colours.sunset name
                                    ]
                            , timerText = timerText secsLeft Colours.sunset
                            , themeColour = Colours.sunset
                            , secsLeft = secsLeft
                            , totalTime = duration
                            }

                nextupString =
                    case List.head <| List.Nonempty.tail workoutData.blocksLeft of
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
                , Element.padding 16
                , Element.behindContent <|
                    Element.el
                        [ Element.alignBottom
                        , Element.width Element.fill
                        , Element.paddingXY 0 16
                        ]
                        (pebbles dataGroup.themeColour
                            { secsLeft = dataGroup.secsLeft
                            , totalTime = dataGroup.totalTime
                            }
                        )
                ]
                [ Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spaceEvenly
                        ]
                        [ Element.el [ Element.height (Element.px 0) ] Element.none
                        , dataGroup.currExercise
                        , Element.el [ Element.centerX ] nextup
                        , Element.el [ Element.height (Element.px 0) ] Element.none
                        ]
                    , Element.el
                        [ Element.centerY
                        , Element.width Element.shrink
                        ]
                      <|
                        Util.viewIcon
                            { icon = playPauseIcon
                            , color = dataGroup.themeColour
                            , size = 75
                            , msg = Just TogglePlay
                            , withBorder = True
                            }
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Util.centerOverlay dataGroup.timerText
                        ]
                        Element.none
                    ]
                ]
    in
    if Util.isVerticalPhone model.device then
        viewPhone

    else
        case model.device.orientation of
            Element.Portrait ->
                Element.text "Portrait mode coming soon lmao"

            Element.Landscape ->
                viewDesktop


viewFinished : Element Msg
viewFinished =
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


viewNeverStarted : Model -> Element Msg
viewNeverStarted model =
    Element.column
        [ Element.width Element.fill
        , Element.centerY
        , Element.spacing 32
        , Element.padding 8
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
            [ Element.spacing 4
            , Element.width Element.fill
            , Font.light
            , Font.center
            , Font.color Colours.sunset
            ]
            [ Element.paragraph
                [ Element.width Element.shrink ]
                [ Element.text "You didn't put anything in your workout!" ]
            , Element.paragraph
                [ Element.width Element.shrink ]
                [ Element.text "Go to the settings and try again." ]
            ]
        ]



---- UPDATE ----


type Msg
    = NewWindowSize Int Int
    | StartExercise Data.WorkoutData
    | NextSecond
    | TogglePlay
    | KeyMsg Keyboard.Msg -- so we can react upon the space key press


update : Msg -> Application -> ( Application, Cmd Msg )
update msg (Application model data) =
    case msg of
        NewWindowSize width height ->
            let
                newWindowSize =
                    Flags.WindowSize width height
            in
            ( Application
                { model
                    | screenDimensions = newWindowSize
                    , device = Element.classifyDevice newWindowSize
                }
                data
            , Cmd.none
            )

        StartExercise workoutData ->
            ( Application model
                { data
                    | state = Data.InProgress workoutData
                    , playing = True
                }
            , Ports.playWhistle ()
            )

        NextSecond ->
            case data.state of
                Data.InProgress workoutData ->
                    let
                        ( block, tl ) =
                            ( List.Nonempty.head workoutData.blocksLeft, List.Nonempty.tail workoutData.blocksLeft )
                    in
                    case Data.decreaseTimeBlock block of
                        Nothing ->
                            case tl of
                                -- no more exercises
                                [] ->
                                    ( Application model { data | state = Data.Finished }, Ports.playTada () )

                                x :: xs ->
                                    ( Application model { data | state = Data.InProgress { workoutData | blocksLeft = Nonempty x xs } }, Ports.playWhistle () )

                        Just newBlock ->
                            let
                                cmd =
                                    if Data.timeLeft newBlock <= 3 then
                                        Ports.playTick ()

                                    else
                                        Cmd.none
                            in
                            ( Application model { data | state = Data.InProgress { workoutData | blocksLeft = Nonempty newBlock tl } }, cmd )

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
                Data.InProgress _ ->
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
