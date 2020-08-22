module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Colours
import Data.Flags exposing (Flags)
import Data.SharedState as SharedState exposing (SharedState)
import Data.Workout as Workout exposing (Workout)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons as Icon
import Page.Home as Home 
import Page.NotFound as NotFound
import FontAwesome.Solid
import FontAwesome.Styles
import Html exposing (Html)
import Http
import Icon
import Page.Home as Home
import Url exposing (Url)
import Util
import Data.Routes as Routes exposing (Route)
import Dict

---- MODEL ----


type alias Model =
    { sharedState : SharedState
    , route : Route 
    , page : Page 

    -- popup letting them know that you can install it as a native app
    , showIosInstall : Bool
    , iosShareIcon : String
    }

-- like Route but also holds the page models
type Page
    = HomePage Home.Model
    | NotFoundPage NotFound.Model


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        sharedState =
            SharedState.init flags navKey
        
        route = 
            Routes.fromUrl url 

        (page, pageCmd) =
            fromRoute route 
    in
    ( { sharedState = sharedState
    , route = route 
    , page = page
      , showIosInstall = flags.showIosInstall
      , iosShareIcon = flags.images.iosShareIconSrc
      }
    , Cmd.batch 
        [Http.get
        { url = "https://joshuaji.com/projects/hiit-timer/version.txt"
        , expect = Http.expectString GotVersion
        }
        , pageCmd]
    )



fromRoute : Route -> ( Page, Cmd Msg )
fromRoute route =
    case route of
        Routes.Home workoutTab ->
            Home.init workoutTab
                |> initWith HomePage HomeMsg

        Routes.NotFound ->
            NotFound.init
                |> initWith NotFoundPage NotFoundMsg



-- initializes a subpage


initWith :
    (subPage -> Page)
    -> (subMsg -> Msg)
    -> ( subPage, Cmd subMsg )
    -> ( Page, Cmd Msg )
initWith toPage toMsg ( route, subCmd ) =
    ( toPage route, Cmd.map toMsg subCmd )


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
        
        content = 
            case model.page of 
                HomePage subModel ->
                    Home.view model.sharedState subModel
                        |> Element.map HomeMsg
                NotFoundPage subModel ->
                    NotFound.view model.sharedState subModel
                        |> Element.map NotFoundMsg

        body =
            [ Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.paddingXY 0 16
                , Element.spacing 32
                ]
                [ content
                ]
                |> Element.layout
                    [ Font.family
                        [ Font.typeface "Lato" ]
                    , Element.inFront <| Element.el [ Element.centerX, Element.padding 8 ] iosInstallPopup
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
    | HomeMsg Home.Msg 
    | NotFoundMsg NotFound.Msg 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (model.page, msg) of
        (_, ClickedLink urlRequest )->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    ,  Nav.pushUrl model.sharedState.navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        (_, UrlChanged url )->
            let

                newRoute =
                    Routes.fromUrl url

                ( newPage, pageCmd ) =
                    fromRoute newRoute
            in
            ( { model
                  | page = newPage
                  , route = newRoute
                }
            , Cmd.none
            )

        (_, GotVersion result )->
            ( { model | sharedState = SharedState.update (SharedState.GotVersion result) model.sharedState }
            , Cmd.none
            )

        (_, NewWindowSize width height )->
            ( { model | sharedState = SharedState.update (SharedState.NewWindowSize width height) model.sharedState }
            , Cmd.none
            )

        (_, RemoveIosInstallPopup )->
            ( { model | showIosInstall = False }
            , Cmd.none
            )
        
        (HomePage subModel, HomeMsg subMsg) ->
            Home.update model.sharedState subMsg subModel 
                |> updateWith HomePage HomeMsg model


        (NotFoundPage subModel, NotFoundMsg subMsg) ->
            NotFound.update model.sharedState subMsg subModel 
                |> updateWith NotFoundPage NotFoundMsg model

        _ ->
            (model, Cmd.none)


-- updates a subpage


updateWith :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toPage toMsg model ( subModel, subMsg ) =
    ( { model | page = toPage subModel }
    , Cmd.map toMsg subMsg
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
