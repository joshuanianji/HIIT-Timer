module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Colours
import Data.Flags exposing (Flags)
import Data.SharedState as SharedState exposing (SharedState)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons as Icon
import FontAwesome.Solid
import FontAwesome.Styles
import Html exposing (Html)
import Http
import Icon
import Url exposing (Url)
import Util
import View.Application as Application exposing (Application)
import View.Config as Config exposing (Config)



---- MODEL ----


type alias Model =
    { sharedState : SharedState
    , workoutTab : WorkoutTab

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


type WorkoutTab
    = Preset
    | Custom


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        sharedState =
            SharedState.init flags navKey

        config =
            Config.init flags
    in
    ( { sharedState = sharedState
      , workoutTab = Preset
      , config = config
      , application = Application.init (Config.getData config) flags
      , showIosInstall = flags.showIosInstall
      , iosShareIcon = flags.images.iosShareIconSrc
      , showSavedCheck = False
      }
    , Http.get
        { url = "https://joshuaji.com/projects/hiit-timer/version.txt"
        , expect = Http.expectString GotVersion
        }
    )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        iosInstallPopup =
            if model.showIosInstall then
                let
                    fontSize =
                        model.sharedState.windowSize.width
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

        body =
            [ Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.paddingXY 0 16
                , Element.spacing 32
                , Element.inFront <| Element.el [ Element.centerX, Element.padding 8 ] iosInstallPopup
                ]
                [ Icon.view
                    [ Element.centerX
                    , Font.size 50
                    ]
                    { icon = FontAwesome.Solid.dumbbell
                    , color = Colours.sunset
                    , size = Icon.Lg
                    }
                , Element.paragraph
                    [ Font.center
                    , Font.color Colours.sunflower
                    , Font.size 50
                    , Font.light
                    ]
                    [ Element.text "HIIT Timer" ]
                , let
                    viewTab ( workoutTab, label ) =
                        let
                            borderColor =
                                if workoutTab == model.workoutTab then
                                    Colours.black

                                else
                                    Colours.transparent
                        in
                        Element.column
                            [ Element.padding 16
                            , Element.spacing 8
                            , Element.pointer
                            , Events.onClick <| ToTab workoutTab
                            ]
                            [ Element.text label
                            , Element.el
                                [ Element.height <| Element.px 5
                                , Background.color borderColor
                                , Element.width Element.fill
                                ]
                                Element.none
                            ]
                  in
                  Element.row
                    [ Element.centerX
                    , Element.spacing 16
                    ]
                  <|
                    List.map viewTab [ ( Preset, "Preset Tabs" ), ( Custom, "Custom Tabs" ) ]
                ]
                |> Element.layout
                    [ Font.family
                        [ Font.typeface "Lato" ]
                    ]
            , FontAwesome.Styles.css
            ]
    in
    { title = "HIIT Timer"
    , body = body
    }



---- UPDATE ----


type Msg
    = ClickedLink UrlRequest
    | UrlChanged Url
    | GotVersion (Result Http.Error String)
    | NewWindowSize Int Int
    | RemoveIosInstallPopup -- ios user clicks the 'x'
    | ToTab WorkoutTab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Cmd.none
                      -- Nav.pushUrl model.sharedState.navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            let
                bruh =
                    "bruh"

                -- newRoute =
                --     Routes.fromUrl url
                -- ( newPage, pageCmd ) =
                --     fromRoute model.sharedState.flags newRoute
            in
            ( model
              -- { model
              --     | page = newPage
              --     , route = newRoute
              --   }
            , Cmd.none
            )

        GotVersion result ->
            ( { model | sharedState = SharedState.update (SharedState.GotVersion result) model.sharedState }
            , Cmd.none
            )

        NewWindowSize width height ->
            ( { model | sharedState = SharedState.update (SharedState.NewWindowSize width height) model.sharedState }
            , Cmd.none
            )

        RemoveIosInstallPopup ->
            ( { model | showIosInstall = False }
            , Cmd.none
            )

        ToTab newTab ->
            ( { model | workoutTab = newTab }
            , Cmd.none
            )



-- helper functions for random crap


subscriptions : Model -> Sub Msg
subscriptions model =
    if Util.isVerticalPhone model.sharedState.device then
        -- vertical phones call a new window size event when the keyboard pops up as well, messing up the view function.
        Sub.none

    else
        Browser.Events.onResize NewWindowSize



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }
