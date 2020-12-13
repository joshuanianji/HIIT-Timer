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
    { version = "Loading..."
    , windowSize = flags.windowSize
    , device = Element.classifyDevice flags.windowSize
    , key = key
    , images = flags.images
    }


type Msg
    = GotVersion (Result Http.Error String)
    | NewWindowSize Int Int


update : Msg -> SharedState -> SharedState
update msg sharedState =
    case msg of
        GotVersion (Ok version) ->
            { sharedState | version = version }

        GotVersion (Err err) ->
            { sharedState | version = Util.httpErrorToString err }

        NewWindowSize width height ->
            { sharedState
                | windowSize = WindowSize width height
                , device = Element.classifyDevice <| WindowSize width height
            }



---- HELPERS ----


navigateTo : Route -> SharedState -> Cmd msg
navigateTo route sharedState =
    Routes.navigateTo sharedState.key route
