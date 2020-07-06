module Main exposing (main)

import Browser
import Browser.Events
import Colours
import Data.Flags as Flags exposing (Flags, WindowSize)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons as Icon
import Html exposing (Html)
import Util
import View.Application as Application exposing (Application)
import View.Config as Config exposing (Config)



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
    { windowSize : WindowSize
    , state : State

    -- internal configuration data
    , config : Config

    -- internal application data
    , application : Application

    -- popup letting them know that you can install it as a native app
    , showIosInstall : Bool
    , iosShareIcon : String

    -- when the local storage is saved, show the checkmark for 2 seconds
    , showSavedCheck : Bool
    }


type State
    = Settings
    | Application


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        config =
            Config.init flags
    in
    ( { windowSize = flags.windowSize
      , state = Application
      , config = config
      , application = Application.init (Config.getData config) flags
      , showIosInstall = flags.showIosInstall
      , iosShareIcon = flags.images.iosShareIconSrc
      , showSavedCheck = False
      }
    , Cmd.none
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        iosInstallPopup =
            if model.showIosInstall then
                let
                    fontSize =
                        model.windowSize.width
                            // 24
                            |> clamp 13 30
                in
                Element.row
                    [ Element.padding 12
                    , Element.spacing 8
                    , Font.size fontSize
                    , Font.light
                    , Background.color Colours.white
                    , Border.color Colours.sunflower
                    , Border.rounded 15
                    , Border.width 1
                    , Border.shadow
                        { offset = ( 0, 0 )
                        , size = 2
                        , blur = 4
                        , color = Colours.withAlpha 0.4 Colours.lightGray
                        }
                    ]
                    [ Element.textColumn
                        [ Element.width Element.fill
                        , Element.spacing 4
                        , Element.paddingXY 8 0
                        ]
                        [ Element.paragraph
                            [ Element.width Element.fill ]
                            [ Element.text "Install this webapp on your iOS device! " ]
                        , Element.paragraph
                            [ Element.width Element.fill ]
                            [ Element.text "In Safari, tap "
                            , Element.image
                                [ Element.height (Element.px fontSize)
                                , Element.paddingXY 2 0
                                ]
                                { src = model.iosShareIcon
                                , description = "iOS Share button"
                                }
                            , Element.text ", then 'Add to the homescreen.'"
                            ]
                        ]
                    , Element.el
                        [ Element.height Element.fill
                        , Element.width <| Element.px 1
                        , Background.color Colours.lightGray
                        ]
                        Element.none
                    , Util.viewIcon
                        { icon = Icon.x
                        , color = Colours.sunset
                        , size = toFloat fontSize * 2
                        , msg = Just RemoveIosInstallPopup
                        , withBorder = False
                        }
                        |> Element.el
                            [ Element.width Element.shrink
                            , Element.centerY
                            ]
                    ]

            else
                Element.none
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.paddingXY 0 16
        , Element.inFront <| Element.el [ Element.centerX, Element.padding 8 ] iosInstallPopup
        ]
        [ case model.state of
            Settings ->
                settings model

            Application ->
                application model
        ]
        -- |> Element.layoutWith
        --     { options =
        --         [ Element.focusStyle
        --             { borderColor = Just Colours.focusBorder
        --             , backgroundColor = Nothing
        --             , shadow =
        --                 Just
        --                     { color = Colours.focusBorder
        --                     , offset = ( 0, 0 )
        --                     , blur = 0
        --                     , size = 3
        --                     }
        --             }
        --         ]
        --     }
        |> Element.layout
            [ Font.family
                [ Font.typeface "Lato" ]
            ]


settings : Model -> Element Msg
settings model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 32
        , Element.paddingXY 0 16
        ]
        [ Config.view model.config
            |> Element.map ConfigMsg

        -- save settings; go to applications as well as save to localhost
        , Util.viewIcon
            { icon = Icon.check
            , color = Colours.grass
            , size = 50
            , msg = Just ToApplication
            , withBorder = True
            }
            |> Util.withTooltip
                { position = Util.Top
                , content = "Finish editing"
                }
            |> Element.el
                [ Element.spacing 16
                , Element.centerX
                ]
        ]


application : Model -> Element Msg
application model =
    let
        applicationView =
            Application.view model.application
                |> Element.map ApplicationMsg

        phoneView =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                [ -- "nav bar"
                  Element.row
                    [ Element.width Element.fill
                    , Element.padding 8
                    , Element.inFront <|
                        Element.el
                            [ Element.alignRight
                            , Element.padding 16
                            ]
                        <|
                            Util.viewIcon
                                { icon = Icon.x
                                , color = Colours.sunset
                                , size = 30
                                , msg = Just ToSettings
                                , withBorder = False
                                }
                    ]
                    [ Element.el [ Element.centerX ] <|
                        Util.viewIcon
                            { icon = Icon.zap
                            , color = Colours.sunset
                            , size = 45
                            , msg = Nothing
                            , withBorder = False
                            }
                    ]
                , applicationView
                ]

        desktopView =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 16
                ]
                [ -- zap icon at the top
                  Util.viewIcon
                    { icon = Icon.zap
                    , color = Colours.sunset
                    , size = 50
                    , msg = Nothing
                    , withBorder = False
                    }
                    |> Element.el [ Element.centerX ]
                , applicationView
                , if Application.exercising model.application then
                    Util.viewIcon
                        { icon = Icon.x
                        , color = Colours.sunset
                        , size = 40
                        , msg = Just ToSettings
                        , withBorder = True
                        }
                        |> Util.withTooltip
                            { position = Util.Top
                            , content = "Exit the workout"
                            }
                        |> Element.el
                            [ Element.centerX
                            , Element.alignBottom
                            ]

                  else
                    Util.viewIcon
                        { icon = Icon.settings
                        , color = Colours.sky
                        , size = 40
                        , msg = Just ToSettings
                        , withBorder = True
                        }
                        |> Element.el
                            [ Element.centerX
                            , Element.alignBottom
                            ]
                ]
    in
    if Util.isVerticalPhone (Element.classifyDevice model.windowSize) then
        phoneView

    else
        desktopView



---- UPDATE ----


type Msg
    = NewWindowSize Int Int
    | RemoveIosInstallPopup -- ios user clicks the 'x'
    | ConfigMsg Config.Msg
    | ApplicationMsg Application.Msg
    | ToApplication
    | ToSettings -- navigate to settings



-- really should use lenses


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewWindowSize width height ->
            ( { model | windowSize = Flags.WindowSize width height }
            , Cmd.none
            )

        RemoveIosInstallPopup ->
            ( { model | showIosInstall = False }
            , Cmd.none
            )

        ConfigMsg configMsg ->
            let
                ( newConfig, configCmd ) =
                    Config.update configMsg model.config
            in
            ( { model | config = newConfig }, Cmd.map ConfigMsg configCmd )

        ApplicationMsg applicationMsg ->
            let
                ( newApp, appCmd ) =
                    Application.update applicationMsg model.application
            in
            ( { model | application = newApp }, Cmd.map ApplicationMsg appCmd )

        ToApplication ->
            ( { model
                | state = Application
                , application = Application.updateData (Config.getData model.config) model.application
              }
            , Cmd.none
            )

        ToSettings ->
            ( { model
                | state = Settings
                , application = Application.endWorkout model.application
              }
            , Cmd.none
            )



-- helper functions for random crap


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        specifics =
            case model.state of
                Settings ->
                    Config.subscriptions model.config
                        |> Sub.map ConfigMsg

                Application ->
                    Application.subscriptions model.application
                        |> Sub.map ApplicationMsg
    in
    Sub.batch
        [ specifics
        , Browser.Events.onResize NewWindowSize
        ]
