module Modules.Application exposing (Application, Msg, exercising, init, subscriptions, update, view)

import Colours
import Data.Application as Data exposing (Data)
import Data.Config
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons as Icon
import List.Nonempty exposing (Nonempty(..))
import Ports
import Time
import Util



-- TYPE


type Application
    = Application Data


init : Data.Config.Data -> Application
init =
    Data.fromConfig >> Application



-- Main.elm uses this to guess whether or not to show the thing at the bottom


exercising : Application -> Bool
exercising (Application data) =
    case data.state of
        Data.InProgress _ ->
            True

        _ ->
            False



-- VIEW


view : Application -> Element Msg
view (Application data) =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 32
        ]
        [ Util.viewIcon
            { icon = Icon.zap
            , color = Colours.sunset
            , size = 50
            , msg = Nothing
            }
            |> Element.el
                [ Element.centerX ]

        -- the main view
        , case data.state of
            Data.Starting blocks ->
                Element.el
                    [ Element.width Element.fill
                    , Element.centerY
                    , Element.above <|
                        Element.el
                            [ Element.centerX
                            , Element.moveUp 100
                            , Font.size 50
                            , Font.center
                            , Font.color Colours.sunset
                            ]
                        <|
                            Element.text "Ready?"
                    ]
                <|
                    (Element.el
                        [ Element.centerX
                        , Element.padding 4
                        ]
                     <|
                        Util.viewIcon
                            { icon = Icon.play
                            , color = Colours.sunset
                            , size = 75
                            , msg = Just <| StartExercise blocks
                            }
                    )

            Data.InProgress blocks ->
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
                            , Font.light
                            , Font.size 100
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

                            Data.Break secsLeft total ->
                                { upperElem = bigFont 32 Colours.grass "Break"
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
                                        [ bigFont 48 Colours.sunflower setName
                                        , bigFont 32 Colours.sunset name
                                        ]
                                , timerText = timerText secsLeft Colours.sunset
                                , timerBar = timerBar secsLeft duration Colours.sunset
                                , theme = Colours.sunset
                                }

                    centerButtonIcon =
                        if data.playing then
                            Icon.pause

                        else
                            Icon.play
                in
                Element.el
                    [ Element.width Element.fill
                    , Element.centerY
                    , Element.above <|
                        Element.el
                            [ Element.moveUp 100
                            , Element.centerX
                            ]
                            dataGroup.upperElem
                    , Element.below <|
                        Element.el
                            [ Element.moveDown 100
                            , Element.centerX
                            ]
                            dataGroup.timerText
                    , Element.inFront <|
                        (Util.viewIcon
                            { icon = centerButtonIcon
                            , color = dataGroup.theme
                            , size = 75
                            , msg = Just TogglePlay
                            }
                            |> Element.el
                                [ Element.centerX
                                , Element.moveUp 58
                                , Element.padding 4
                                , Background.color Colours.white
                                ]
                        )
                    ]
                    dataGroup.timerBar
                    |> Util.surround 1 4 1

            Data.Finished ->
                Element.column
                    [ Element.width <| Element.fillPortion 5
                    , Element.centerY
                    , Element.spacing 32
                    ]
                    [ Util.viewIcon
                        { icon = Icon.star
                        , color = Colours.sunflower
                        , size = 100
                        , msg = Nothing
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
                        [ Element.text "Congradulations! You finished!" ]
                    ]
        ]



-- UPDATE


type Msg
    = StartExercise (Nonempty Data.TimeBlock)
    | NextSecond
    | TogglePlay


update : Msg -> Application -> ( Application, Cmd Msg )
update msg (Application data) =
    case msg of
        StartExercise blocks ->
            ( Application
                { data
                    | state = Data.InProgress blocks
                    , playing = True
                }
            , Ports.playWhistle ()
            )

        NextSecond ->
            case data.state of
                Data.InProgress (Nonempty block tl) ->
                    case Data.decreaseTimeBlock block of
                        Nothing ->
                            case tl of
                                -- no more exercises
                                [] ->
                                    ( Application { data | state = Data.Finished }, Ports.playTada () )

                                x :: xs ->
                                    ( Application { data | state = Data.InProgress <| Nonempty x xs }, Ports.playWhistle () )

                        Just newBlock ->
                            ( Application { data | state = Data.InProgress <| Nonempty newBlock tl }, Cmd.none )

                _ ->
                    -- ignore
                    ( Application data, Cmd.none )

        TogglePlay ->
            ( Application { data | playing = not data.playing }, Ports.playWhistle () )



-- SUBSCRIPTIONS


subscriptions : Application -> Sub Msg
subscriptions (Application data) =
    case data.state of
        Data.InProgress _ ->
            if data.playing then
                Time.every 1000 (always NextSecond)

            else
                Sub.none

        _ ->
            Sub.none
