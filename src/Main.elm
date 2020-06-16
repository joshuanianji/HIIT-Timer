module Main exposing (application, main)

import Browser
import Colours
import Data.Duration as Duration exposing (Duration)
import Data.Flags exposing (Flags, WindowSize)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import FeatherIcons as Icon
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Modules.Application as Application exposing (Application)
import Modules.Config as Config exposing (Config)
import Modules.Exercise as Exercise exposing (Exercise)
import Modules.Set as Set exposing (Set)
import Modules.TimeInput as TimeInput exposing (TimeInput)
import Ports
import Process
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

    -- internal application data
    , application : Application

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
            Config.init flags.storedConfig
    in
    ( { time = Time.millisToPosix flags.posix
      , zone = Time.utc
      , windowSize = flags.windowSize
      , state = Application
      , config = config
      , application = Application.init (Config.getData config)
      , showSavedCheck = False
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
                        , msg = Just ToApplication
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
        , Element.spacing 24
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
                settings model

            Application ->
                application model
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


settings : Model -> Element Msg
settings model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Config.view model.config
            |> Element.map ConfigMsg

        -- save settings; go to applications as well as save to localhost
        , Element.row
            [ Element.spacing 32
            , Element.centerX
            ]
            [ Util.viewIcon
                { icon = Icon.check
                , color = Colours.grass
                , size = 50
                , msg = Just ToApplication
                }
            , Element.el
                [ Element.onRight <|
                    if model.showSavedCheck then
                        Element.row
                            [ Element.spacing 4
                            , Element.padding 4
                            , Element.centerY
                            , Font.color Colours.grass
                            , Font.light
                            ]
                            [ Util.viewIcon
                                { icon = Icon.check
                                , color = Colours.grass
                                , size = 20
                                , msg = Nothing
                                }
                            , Element.text "Settings saved to Local Storage"
                            ]

                    else
                        Element.none
                ]
              <|
                Util.viewIcon
                    { icon = Icon.save
                    , color = Colours.sky
                    , size = 50
                    , msg = Just ToLocalStorage
                    }
            ]
        ]


application : Model -> Element Msg
application model =
    Application.view model.application



---- UPDATE ----


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | ConfigMsg Config.Msg
    | ToApplication
    | ToSettings -- navigate to settings
    | ToLocalStorage -- save to local storage
    | StoreConfigSuccess -- when local storage succeeds
    | RemoveSavedCheck



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

        ConfigMsg configMsg ->
            ( { model | config = Config.update configMsg model.config }, Cmd.none )

        ToApplication ->
            ( { model
                | state = Application
                , application = Application.init (Config.getData model.config)
              }
            , Cmd.none
            )

        ToSettings ->
            ( { model | state = Settings }
            , Cmd.none
            )

        ToLocalStorage ->
            ( model, Ports.storeConfig (Config.encode model.config) )

        StoreConfigSuccess ->
            ( { model | showSavedCheck = True }
            , Process.sleep 2000
                |> Task.perform (\_ -> RemoveSavedCheck)
            )

        RemoveSavedCheck ->
            ( { model | showSavedCheck = False }, Cmd.none )



-- helper functions for random crap


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Ports.storeConfigSuccess <| always StoreConfigSuccess
        ]
