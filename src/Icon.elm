module Icon exposing (IconSize(..), faSize, view)

-- helping me view FontAwesome icons

import Colours
import Element exposing (Color, Element)
import Element.Font as Font
import FontAwesome.Attributes as Attrs
import FontAwesome.Icon exposing (Icon)


type IconSize
    = Xs
    | Sm
    | Lg
    | Fa Int


faSize : Int -> IconSize
faSize n =
    Fa <| clamp 2 10 n


view : List (Element.Attribute msg) -> { icon : Icon, color : Color, size : IconSize } -> Element msg
view attrs data =
    let
        attr =
            case data.size of
                Xs ->
                    Attrs.xs

                Sm ->
                    Attrs.sm

                Lg ->
                    Attrs.lg

                Fa 2 ->
                    Attrs.fa2x

                Fa 3 ->
                    Attrs.fa3x

                Fa 4 ->
                    Attrs.fa4x

                Fa 5 ->
                    Attrs.fa5x

                Fa 6 ->
                    Attrs.fa6x

                Fa 7 ->
                    Attrs.fa7x

                Fa 8 ->
                    Attrs.fa8x

                Fa 9 ->
                    Attrs.fa9x

                Fa 10 ->
                    Attrs.fa10x

                _ ->
                    Attrs.xs
    in
    FontAwesome.Icon.viewStyled [ attr ] data.icon
        |> Element.html
        |> Element.el
            (Font.color data.color :: attrs)
