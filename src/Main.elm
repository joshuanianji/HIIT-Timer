module Main exposing (main)

import Browser
import Element
import Element.Background as Background
import Html exposing (Html)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



---- MODEL ----


type alias Model =
    {}


init : Model
init =
    {}



---- VIEW ----


view : Model -> Html Msg
view _ =
    Element.column
        [ -- our "popup"
          Element.inFront <|
            Element.el
                [ Element.width (Element.px 600)
                , Background.color <| Element.rgb 1 0 0
                , Element.height (Element.px 100)
                ]
                Element.none
        ]
        [ -- using onRight or onLeft STILL SHOWS ABOVE THE POPUP
          Element.el
            [ Element.onRight <|
                Element.text "Hello"
            , Element.onLeft <| Element.text "bruh"

            -- doing this FIXES IT
            , Element.moveDown 0
            , Element.height (Element.px 20)
            ]
            Element.none

        -- this is fine
        , Element.text "hello"
        ]
        |> Element.layout []



---- UPDATE ----


type Msg
    = NoOp



-- really should use lenses


update : Msg -> Model -> Model
update _ model =
    model
