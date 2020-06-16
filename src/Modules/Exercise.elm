module Modules.Exercise exposing (Exercise, breakView, essentials, fromData, getData, init, updateName, view)

import Colours
import Data.Duration as Duration exposing (Duration)
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icon
import Util


type Exercise
    = Exercise Data



-- internal data (doesn't rely on external sources for example the duration)


type alias Data =
    { position : Int
    , name : String
    }



-- user input when view


type alias Options msg =
    { onDelete : Int -> msg
    , updateName : Int -> String -> msg
    , duration : Duration
    }



-- takes an int to initialize with a number (Exercise1, Exercise2)


init : Int -> Exercise
init n =
    Exercise
        { position = n
        , name = "Exercise " ++ String.fromInt n
        }



-- HELPERS


essentials : Duration -> Exercise -> ( String, Duration )
essentials duration (Exercise data) =
    ( data.name, duration )



-- for localstorage dudes


getData : Exercise -> Data
getData (Exercise data) =
    data


fromData : Data -> Exercise
fromData =
    Exercise



-- VIEW


view : Options msg -> Exercise -> Element msg
view options (Exercise data) =
    Element.row
        [ Element.spacing 8
        , Element.padding 16
        , Element.width Element.fill
        , Border.rounded 4
        , Border.width 1
        , Border.color Colours.sunset
        , Font.color Colours.sunset
        ]
        [ Element.column
            [ Element.spacing 8
            , Element.width Element.fill
            ]
            [ Input.text
                [ Font.size 28
                , Border.color Colours.white
                , Element.padding 0
                , Element.mouseOver
                    [ Border.color Colours.sunset ]
                ]
                { onChange = options.updateName data.position
                , text = data.name
                , placeholder = Nothing
                , label = Input.labelHidden "New name for set"
                }
            , Duration.viewFancy options.duration
            ]
        , Util.viewIcon
            { icon = Icon.trash
            , color = Colours.sunset
            , size = 25
            , msg = Just (options.onDelete data.position)
            }
            |> Element.el
                [ Element.alignRight
                , Element.centerY
                ]
        ]


breakView : Duration -> Element msg
breakView duration =
    Element.row
        [ Element.padding 16
        , Element.spacing 16
        , Element.width Element.fill
        , Border.width 1
        , Border.color Colours.grass
        , Border.rounded 4
        , Font.color Colours.grass
        , Font.light
        ]
        [ Element.text "Break:"
        , Duration.viewFancy duration
        ]



-- UPDATE


updateName : String -> Exercise -> Exercise
updateName newName (Exercise data) =
    Exercise { data | name = newName }
