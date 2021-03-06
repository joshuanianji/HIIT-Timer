module Data.TimeBlock exposing (TimeBlock(..), decreaseSecond, timeLeft)

---- TYPE ----


type TimeBlock
    = ExerciseBreak Int Int
    | SetBreak Int Int
    | CountDown Int Int
    | Exercise
        { setName : String
        , name : String
        , duration : Int
        , secsLeft : Int
        }


timeLeft : TimeBlock -> Int
timeLeft block =
    case block of
        ExerciseBreak remaining _ ->
            remaining

        SetBreak remaining _ ->
            remaining

        CountDown remaining _ ->
            remaining

        Exercise data ->
            data.secsLeft



-- if the timeblock ends we return Nothing


decreaseSecond : TimeBlock -> Maybe TimeBlock
decreaseSecond tb =
    case tb of
        ExerciseBreak curr total ->
            if curr <= 1 then
                Nothing

            else
                Just <| ExerciseBreak (curr - 1) total

        SetBreak curr total ->
            if curr <= 1 then
                Nothing

            else
                Just <| SetBreak (curr - 1) total

        CountDown curr total ->
            if curr <= 1 then
                Nothing

            else
                Just <| CountDown (curr - 1) total

        Exercise dater ->
            if dater.secsLeft <= 1 then
                Nothing

            else
                Just <| Exercise { dater | secsLeft = dater.secsLeft - 1 }
