module Data.Flags exposing (Flags, WindowSize)

import Json.Encode



---- TYPE ----


type alias Flags =
    { windowSize : WindowSize
    , storedConfig : Json.Encode.Value
    , smhSrc : String
    }



---- HELPERS ----


type alias WindowSize =
    { width : Int
    , height : Int
    }
