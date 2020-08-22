module Data.Routes exposing (Route(..), WorkoutTab(..), fromUrl, toUrlString)

import Url 
import Url.Parser as Parser exposing (Parser, (<?>))
import Url.Parser.Query as Query 
import Url.Builder as Builder 
import Dict

type Route 
    = Home WorkoutTab
    | NotFound

type WorkoutTab
    = Preset
    | Custom


-- PARSER 



fromUrl : Url.Url -> Route
fromUrl url =
    url
        |> Parser.parse urlParser
        |> Maybe.withDefault NotFound


toUrlString : Route -> String
toUrlString route =
    let
        name =
            case route of
                Home tab ->
                    Builder.absolute [] [tabToUrl tab]

                NotFound ->
                    Builder.absolute ["not-found"] []
    in
    name



-- INTERNAL


urlParser : Parser (Route -> a) a
urlParser =
    Parser.oneOf
        [ Parser.map Home (Parser.top <?> tabParser)
        ]


tabParser : Query.Parser WorkoutTab
tabParser =
    Query.enum "tab" (Dict.fromList [("custom", Custom), ("preset", Preset)])
        |> Query.map (Maybe.withDefault Preset)


tabToUrl : WorkoutTab -> Builder.QueryParameter
tabToUrl tab =
    case tab of 
        Preset  -> 
            Builder.string "tab" "preset"

        Custom -> 
            Builder.string "tab" "custom"

