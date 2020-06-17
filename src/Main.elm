module Main exposing (application, main)

import Browser
import Colours
import Data.Flags exposing (Flags, WindowSize)
import Element exposing (Element)
import Element.Font as Font
import FeatherIcons as Icon
import Html exposing (Html)
import Modules.Application as Application exposing (Application)
import Modules.Config as Config exposing (Config)
import Ports
import Process
import Task
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
    { windowSize : WindowSize
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
    ( { windowSize = flags.windowSize
      , state = Settings
      , config = config
      , application = Application.init (Config.getData config)
      , showSavedCheck = False
      }
    , Cmd.none
    )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 16
        , Element.spacing 24
        ]
        [ case model.state of
            Settings ->
                settings model

            Application ->
                application model
        ]
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
                |> Util.withTooltip
                    { position = Util.Top
                    , content = "Finish editing"
                    }
            , Util.viewIcon
                { icon = Icon.save
                , color = Colours.sky
                , size = 50
                , msg = Just ToLocalStorage
                }
                |> Element.el
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
                |> Util.withTooltip
                    { position = Util.Top
                    , content = "Save configuration to Local Storage"
                    }
            ]
        ]


application : Model -> Element Msg
application model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront <|
            if Application.exercising model.application then
                Element.none

            else
                Util.viewIcon
                    { icon = Icon.settings
                    , color = Colours.sky
                    , size = 50
                    , msg = Just ToSettings
                    }
                    |> Element.el
                        [ Element.centerX
                        , Element.alignBottom
                        ]
        ]
        [ Application.view model.application
            |> Element.map ApplicationMsg
        ]



---- UPDATE ----


type Msg
    = ConfigMsg Config.Msg
    | ApplicationMsg Application.Msg
    | ToApplication
    | ToSettings -- navigate to settings
    | ToLocalStorage -- save to local storage
    | StoreConfigSuccess -- when local storage succeeds
    | RemoveSavedCheck



-- really should use lenses


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConfigMsg configMsg ->
            ( { model | config = Config.update configMsg model.config }, Cmd.none )

        ApplicationMsg applicationMsg ->
            let
                ( newApp, appCmd ) =
                    Application.update applicationMsg model.application
            in
            ( { model | application = newApp }, Cmd.map ApplicationMsg appCmd )

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
subscriptions model =
    Sub.batch
        [ Application.subscriptions model.application
            |> Sub.map ApplicationMsg
        , Ports.storeConfigSuccess <| always StoreConfigSuccess
        ]
