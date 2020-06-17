module Util exposing (Position(..), surround, unselectable, viewIcon, withTooltip)

-- misc functions

import Colours
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons as Icon
import Html.Attributes


surround : Int -> Int -> Int -> Element msg -> Element msg
surround left middle right =
    \el ->
        Element.row
            [ Element.width Element.fill, Element.height Element.fill ]
            [ Element.el [ Element.width <| Element.fillPortion left, Element.height Element.fill ] Element.none
            , Element.el [ Element.width <| Element.fillPortion middle, Element.height Element.fill ] el
            , Element.el [ Element.width <| Element.fillPortion right, Element.height Element.fill ] Element.none
            ]


viewIcon : { icon : Icon.Icon, color : Element.Color, size : Float, msg : Maybe msg } -> Element msg
viewIcon data =
    let
        icon =
            data.icon
                |> Icon.withStrokeWidth 1
                |> Icon.withSize data.size
                |> Icon.toHtml []
                |> Element.html

        similarAttrs =
            [ Font.color data.color
            , Element.padding (round data.size // 4)
            , Border.rounded (round data.size)
            ]
    in
    case data.msg of
        -- don't make it interactive
        Nothing ->
            Element.el similarAttrs icon

        Just msg ->
            Element.el
                ([ Element.pointer
                 , Border.width 1
                 , Border.color data.color
                 , Element.mouseOver
                    [ Font.color Colours.white
                    , Background.color data.color
                    ]
                 , Events.onClick msg
                 ]
                    ++ similarAttrs
                )
                icon


unselectable : Element.Attribute msg
unselectable =
    Element.htmlAttribute (Html.Attributes.class "noselect")


withTooltip : { position : Position, content : String } -> Element msg -> Element msg
withTooltip { position, content } =
    Element.el
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.htmlAttribute <| Html.Attributes.class (positionToClass position)
        , Element.htmlAttribute <| Html.Attributes.attribute "data-tooltip" content
        ]


type Position
    = Top
    | Bottom
    | Left
    | Right



-- internal tooltip stuff


positionToClass : Position -> String
positionToClass p =
    let
        str =
            case p of
                Top ->
                    "top"

                Bottom ->
                    "bottom"

                Left ->
                    "left"

                Right ->
                    "right"
    in
    "simptip-position-" ++ str
