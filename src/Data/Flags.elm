module Data.Flags exposing (Flags, WindowSize)


type alias Flags =
    { windowSize : WindowSize
    , posix : Int
    }


type alias WindowSize =
    { height : Int
    , width : Int
    }
