module Data.Flags exposing (Flags, WindowSize)

import Json.Encode


type alias Flags =
    { windowSize : WindowSize
    , storedConfig : Json.Encode.Value
    }


type alias WindowSize =
    { height : Int
    , width : Int
    }
