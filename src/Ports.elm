port module Ports exposing (storeConfig, storeConfigSuccess)

import Json.Encode


port storeConfig : Json.Encode.Value -> Cmd msg


port storeConfigSuccess : (() -> msg) -> Sub msg
