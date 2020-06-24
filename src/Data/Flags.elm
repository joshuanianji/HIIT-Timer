module Data.Flags exposing (Flags, WindowSize)

import Json.Encode


type alias Flags =
    { windowSize : WindowSize
    , storedConfig : Json.Encode.Value
    , smhSrc : String
    }


type alias WindowSize =
    { height : Int
    , width : Int
    }
