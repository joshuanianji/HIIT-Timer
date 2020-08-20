module Data.SharedState exposing (Msg(..), SharedState, init, update)

import Browser.Navigation as Nav
import Data.Flags exposing (Flags, WindowSize)
import Element
import Http


type alias SharedState =
    { version : String
    , windowSize : WindowSize
    , device : Element.Device
    , navKey : Nav.Key
    }



-- when we init we don't have the version number yet - this comes with the HTTP request


init : Flags -> Nav.Key -> SharedState
init flags navKey =
    { version = "Loading..."
    , windowSize = flags.windowSize
    , device = Element.classifyDevice flags.windowSize
    , navKey = navKey
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
            { sharedState | version = httpErrorToString err }

        NewWindowSize width height ->
            { sharedState
                | windowSize = WindowSize width height
                , device = Element.classifyDevice <| WindowSize width height
            }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "BadUrl! " ++ url

        Http.Timeout ->
            "Timeout!"

        Http.NetworkError ->
            "NetworkError!"

        Http.BadStatus status ->
            "BadStatus! " ++ String.fromInt status

        Http.BadBody body ->
            "BadBody! " ++ body
