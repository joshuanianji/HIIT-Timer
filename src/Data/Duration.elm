module Data.Duration exposing (Duration, add, fromString, init, sanitize, toSeconds, toString, viewFancy)

import Element exposing (Element)
import Element.Font as Font


type alias Duration =
    { minutes : Int
    , seconds : Int
    }


init : Int -> Duration
init secs =
    { minutes = 0
    , seconds = secs
    }
        |> sanitize



-- make seconds under 60


sanitize : Duration -> Duration
sanitize { minutes, seconds } =
    let
        newSecs =
            seconds |> modBy 60

        newMins =
            minutes + (seconds // 60)
    in
    { minutes = newMins
    , seconds = newSecs
    }



-- turns it into pure seconds (for exercise recursion when doing countdown)


toSeconds : Duration -> Int
toSeconds { minutes, seconds } =
    minutes * 60 + seconds



-- 3 minutes 20 seconds => 320 (used for time input)


toString : Duration -> String
toString { minutes, seconds } =
    String.fromInt minutes ++ (String.pad 2 '0' <| String.fromInt seconds)


{-| a string of RAW numbers (right to left)
e.g 345 => 3:45
1223 => 12:23
199 => 2:39

    has to be below 4 digits (i'll just take the rightmost 4)

-}
fromString : String -> Maybe Duration
fromString =
    String.right 4
        >> (\s -> ( String.dropRight 2 s, String.right 2 s ))
        >> Tuple.mapBoth String.toInt String.toInt
        >> Tuple.mapFirst (Maybe.withDefault 0)
        >> (\( mins, secs ) -> Maybe.map (Duration mins) secs)



-- addition


add : Duration -> Duration -> Duration
add d1 d2 =
    { minutes = d1.minutes + d2.minutes
    , seconds = d1.seconds + d2.seconds
    }
        |> sanitize



-- fancy view


viewFancy : Duration -> Element msg
viewFancy { minutes, seconds } =
    Element.row
        [ Element.spacing 4 ]
        [ Element.row
            [ Element.spacing 2
            , Font.light
            , Element.alignBottom
            ]
            [ Element.text <| String.fromInt minutes
            , Element.el
                [ Font.size 12
                , Element.alignBottom
                ]
              <|
                Element.text "m"
            ]
        , Element.row
            [ Element.spacing 2
            , Font.light
            ]
            [ Element.text <| String.padLeft 2 '0' <| String.fromInt seconds
            , Element.el
                [ Font.size 12
                , Element.alignBottom
                ]
              <|
                Element.text "s"
            ]
        ]
