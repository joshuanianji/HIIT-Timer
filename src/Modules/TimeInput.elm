module Modules.TimeInput exposing (TimeInput, getDuration, init, updateFocus, updateInput, view)

import Colours
import Data.Duration as Duration exposing (Duration)
import Element exposing (Element)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Util


type TimeInput
    = TimeInput Data



-- internal data


type alias Data =
    { input : String
    , duration : Duration
    , focused : Bool
    }



-- user puts these in when they run view function


type alias Options msg =
    { updateInput : String -> msg
    , updateFocus : Bool -> msg
    , displayText : Maybe String
    }



-- initialize with seconds


init : Int -> TimeInput
init secs =
    let
        duration =
            Duration.init secs
    in
    TimeInput
        { input = Duration.toString duration
        , duration = duration
        , focused = False
        }



-- HELPER FUNCTIONS


getDuration : TimeInput -> Duration
getDuration (TimeInput data) =
    data.duration



-- VIEW


view : Options msg -> TimeInput -> Element msg
view options (TimeInput data) =
    Element.row
        [ Element.spacing 8
        , Element.centerX
        ]
        [ Input.text
            [ Element.width (Element.px 175)
            , Element.padding 24

            -- FONT SIZE 0 HIDES THE STUPID UGLY BEHIND-THE-SCENES TEXT
            , Font.size 0

            -- it's weird how inFront works better  - if i write behindContent, it'll take two clicks to focus on the input.
            , Element.inFront <|
                -- if it's not focused, we would have gotten the new sanitized object
                Element.row
                    [ Element.padding 8
                    , Element.spacing 3
                    , Element.centerY
                    , Font.size 20
                    , Element.alignRight
                    , Font.color Colours.black

                    -- cursor
                    , Element.inFront <|
                        if data.focused then
                            Element.el
                                [ Element.centerY
                                , Element.padding 8
                                , Element.height Element.fill
                                , Element.alignRight
                                ]
                                (Element.el
                                    [ Element.height Element.fill
                                    , Element.width (Element.px 1)
                                    , Background.color Colours.black
                                    ]
                                    Element.none
                                )

                        else
                            Element.none
                    ]
                    [ Duration.viewFancy data.duration ]
            , Events.onFocus <| options.updateFocus True
            , Events.onLoseFocus <| options.updateFocus False

            -- so we don't see the ugly underlying text
            , Font.color Colours.transparent
            , Util.unselectable
            ]
            { onChange = options.updateInput
            , text = data.input
            , placeholder = Nothing
            , label =
                Input.labelLeft
                    [ Element.centerY
                    , Element.centerX
                    , Element.padding 4
                    , Font.light
                    , Font.size 20
                    ]
                <|
                    (options.displayText
                        |> Maybe.map Element.text
                        |> Maybe.withDefault Element.none
                    )
            }
        ]



-- UPDATE


updateInput : TimeInput -> String -> TimeInput
updateInput (TimeInput data) newInput =
    let
        sanitizedInput =
            newInput
                |> String.filter Char.isDigit
                |> String.left 4
    in
    TimeInput
        { data
            | input = sanitizedInput
            , duration =
                Duration.fromString sanitizedInput
                    |> Maybe.withDefault (Duration.init 0)
        }


updateFocus : TimeInput -> Bool -> TimeInput
updateFocus (TimeInput data) isFocused =
    if isFocused then
        TimeInput
            { data | focused = True }

    else
        let
            sanitizedDuration =
                Duration.fromString data.input
                    |> Maybe.withDefault (Duration.init 0)
                    |> Duration.sanitize
        in
        -- sanitize data
        TimeInput
            { data
                | focused = False
                , duration = sanitizedDuration
                , input = Duration.toString sanitizedDuration
            }
