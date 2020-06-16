module Modules.Application exposing (Application, init, view)

import Browser
import Colours
import Data.Application as Data exposing (Data)
import Data.Config
import Data.Duration as Duration exposing (Duration)
import Data.Flags exposing (Flags, WindowSize)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import FeatherIcons as Icon
import Html exposing (Html)
import Util



-- type


type Application
    = Application Data


init : Data.Config.Data -> Application
init =
    Data.fromConfig >> Application



-- view


view : Application -> Element msg
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
            Data.InProgress exercisingState ->
                let
                    ( upperElem, bottomElem ) =
                        case exercisingState of
                            Data.CountingDown secs ->
                                ( Element.el
                                    [ Element.centerX
                                    , Font.size 32
                                    , Font.center
                                    , Font.color Colours.sky
                                    ]
                                  <|
                                    Element.text "Countdown"
                                , Element.el
                                    [ Element.centerX
                                    , Font.color Colours.sky
                                    , Font.size 100
                                    ]
                                  <|
                                    Element.text <|
                                        String.fromInt secs
                                )

                            Data.Exercising setNum exerciseNum nonEmpty ->
                                ( Element.none, Element.none )

                    display colour name currN totalN pp =
                        Element.column
                            [ Element.centerX
                            , Font.color colour
                            , Font.light
                            ]
                            [ Element.el [ Font.size 32, Font.center ] <| Element.text name
                            , Element.paragraph
                                [ Font.center ]
                                [ Element.el [ Font.color Colours.black ] <| Element.text (pp ++ " ")
                                , Element.text <| String.fromInt currN
                                , Element.el [ Font.color Colours.black ] <| Element.text " of "
                                , Element.text <| String.fromInt totalN
                                ]
                            ]

                    ( centerButtonIcon, buttonColour ) =
                        if data.playing then
                            ( Icon.pause, Colours.sunset )

                        else
                            ( Icon.play, Colours.sky )
                in
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing 48
                    , Element.centerY
                    ]
                    [ upperElem
                    , Util.viewIcon
                        { icon = centerButtonIcon
                        , color = buttonColour
                        , size = 75
                        , msg = Nothing
                        }
                        |> Element.el
                            [ Element.centerX ]
                    , bottomElem
                    ]

            Data.Finished ->
                Element.column
                    [ Element.width <| Element.fillPortion 5
                    , Element.centerY
                    , Element.spacing 32
                    ]
                    [ Util.viewIcon
                        { icon = Icon.star
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
                        [ Element.text "WOOHOO!" ]
                    ]
        ]
