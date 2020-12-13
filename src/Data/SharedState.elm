module Data.SharedState exposing (Msg(..), SharedState, init, navigateTo, update)

import Browser.Navigation as Nav
import Data.Flags exposing (Flags, Images, WindowSize)
import Element
import Http
import Routes exposing (Route)
import Util


type alias SharedState =
    { version : String
    , windowSize : WindowSize
    , device : Element.Device
    , key : Nav.Key
    , images : Images
    }



-- when we init we don't have the version number yet - this comes with the HTTP request


init : Flags -> Nav.Key -> SharedState
init flags key =
    { version = Maybe.withDefault "build..." flags.version
    , windowSize = flags.windowSize
    , device = Element.classifyDevice flags.windowSize
    , key = key
    , images = flags.images
    }


type Msg
    = NewWindowSize Int Int


update : Msg -> SharedState -> SharedState
update msg sharedState =
    case msg of
        NewWindowSize width height ->
            { sharedState
                | windowSize = WindowSize width height
                , device = Element.classifyDevice <| WindowSize width height
            }



---- HELPERS ----


navigateTo : Route -> SharedState -> Cmd msg
navigateTo route sharedState =
    Routes.navigateTo sharedState.key route
