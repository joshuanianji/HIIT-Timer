module Data.Flags exposing (Flags, WindowSize)

import Json.Encode



---- TYPE ----


type alias Flags =
    { windowSize : WindowSize
    , storedConfig : Json.Encode.Value
    , showIosInstall : Bool
    , images : Images
    }



---- HELPERS ----


type alias Images =
    { smhSrc : String
    , iosShareIconSrc : String
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }
