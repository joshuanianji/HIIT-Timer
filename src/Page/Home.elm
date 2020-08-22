module Page.Home exposing (Model, Msg, init, update, view)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Colours
import Data.Flags exposing (Flags)
import Data.SharedState exposing (SharedState)
import Data.Workout exposing (Workout)
import Data.Routes as Routes exposing (Route)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons as Icon
import Dict
import FontAwesome.Solid
import Icon
import Url exposing (Url)
import Data.Routes as Routes exposing (WorkoutTab)

---- MODEL ----


type alias Model =
    { workoutTab : WorkoutTab }



init : WorkoutTab -> (Model, Cmd Msg)
init tab =
    ({ workoutTab = tab  }, Cmd.none)



---- VIEW ----


view : SharedState -> Model -> Element Msg
view sharedState model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.paddingXY 0 16
        , Element.spacing 32
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
                    , Events.onClick <| ToTab ( workoutTab) 
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
            List.map viewTab [ ( Routes.Preset, "Preset Workouts" ), ( Routes.Custom, "Custom Workouts" ) ]
        , case model.workoutTab of
            Routes.Preset ->
                preset []

            Routes.Custom ->
                custom []
        ]


preset : List Workout -> Element Msg
preset workouts =
    Element.el
        []
    <|
        Element.text "Preset workouts coming soon!"


custom : List Workout -> Element Msg
custom workouts =
    Element.el
        []
    <|
        Element.text "Make your own workouts"



---- UPDATE ----


type Msg
    = NavigateTo Route
    | ToTab WorkoutTab


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg )
update sharedState msg model =
    case msg of
        ToTab newTab ->
            -- does not enter new entry to browser history
            ( model
            , Nav.replaceUrl sharedState.navKey (Routes.toUrlString <| Routes.Home newTab) 
            )
        

        NavigateTo route ->
            ( model, Nav.pushUrl sharedState.navKey (Routes.toUrlString route) )
