module Colours exposing (black, focusBorder, grass, sky, sunflower, sunset, transparent, white)

-- color scheme based off of "Primary Colours with Vibrant Twist" from https://www.canva.com/learn/100-color-combinations/

import Element exposing (Color)


sky : Color
sky =
    Element.rgb255 55 94 151


sunset : Color
sunset =
    Element.rgb255 251 101 66


sunflower : Color
sunflower =
    Element.rgb255 255 187 0


grass : Color
grass =
    Element.rgb255 63 104 28



-- other


white : Color
white =
    Element.rgb 1 1 1


black : Color
black =
    Element.rgb 0 0 0


transparent : Color
transparent =
    Element.rgba 0 0 0 0


focusBorder : Color
focusBorder =
    Element.rgb255 154 203 255
