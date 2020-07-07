module View.Config exposing
    ( Config
    , Msg
    , encode
    , getData
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Colours
import Data.Config as Data
import Data.Duration as Duration
import Data.Flags as Flags exposing (Flags)
import Dialog
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import FeatherIcons as Icon
import Html.Attributes
import Json.Decode
import Json.Encode
import Modules.Exercise as Exercise
import Modules.Set as Set
import Ports
import Util
import View.TimeInput as TimeInput



---- TYPE ----


type Config
    = Config Model Data.Data


type alias Model =
    { device : Element.Device
    , saving : Bool
    , speechSynthesisPopup : Bool
    }



-- INIT AND JSON STUFF


init : Flags -> Config
init flags =
    let
        decodeLocalStorageAttempt =
            Data.decodeLocalStorage flags.storedConfig

        ( data, mErr ) =
            case decodeLocalStorageAttempt of
                -- there was no config stored in the first place
                Ok Nothing ->
                    ( Data.default, Nothing )

                -- success
                Ok (Just config) ->
                    ( Data.fromLocalStorage config, Nothing )

                -- failure to decode
                Err jsonErr ->
                    ( Data.default, Just <| Json.Decode.errorToString jsonErr )

        actualData =
            { data | error = mErr }

        model =
            { device = Element.classifyDevice flags.windowSize
            , saving = False
            , speechSynthesisPopup = False
            }
    in
    Config model { actualData | error = mErr }



-- Public Helpers


encode : Config -> Json.Encode.Value
encode (Config _ data) =
    Data.encode data


getData : Config -> Data.Data
getData (Config _ data) =
    data



---- VIEW ----


view : Config -> Element Msg
view (Config model data) =
    let
        paddingX =
            case model.device.class of
                Element.Phone ->
                    4

                _ ->
                    32
    in
    Element.column
        [ Element.width (Element.fill |> Element.maximum 1000)
        , Element.centerX
        , Element.height Element.fill
        , Element.paddingXY paddingX 32
        , Element.spacing 52
        , Element.inFront <| speechSynthesisPopup model
        ]
        [ Util.viewIcon
            { icon = Icon.settings
            , color = Colours.sky
            , size = 50
            , msg = Nothing
            , withBorder = False
            }
            |> Element.el [ Element.centerX ]
        , data.error
            |> Maybe.map Element.text
            |> Maybe.withDefault Element.none
        , durations model data
        , viewSounds model data
        , viewSets model data

        -- If the settings is saved or not
        , Element.el
            [ Element.centerX ]
          <|
            if model.saving then
                Element.el
                    [ Font.light
                    , Font.color Colours.sunflower
                    ]
                <|
                    Element.text "saving..."

            else
                Element.el
                    [ Font.light
                    , Font.color Colours.grass
                    ]
                <|
                    Element.text "All Changes Saved to Local Storage "
        ]


durations : Model -> Data.Data -> Element Msg
durations model data =
    Element.column
        [ Element.centerX
        , Element.spacing 12
        ]
        [ Util.viewIcon
            { icon = Icon.clock
            , color = Colours.sunset
            , size = 40
            , msg = Nothing
            , withBorder = False
            }
        , TimeInput.view
            { displayText = Just "Exercise Duration:"
            , device = model.device
            }
            data.exerciseInput
            |> Element.map (UpdateTimeInput ExerciseIpt)
        , TimeInput.view
            { displayText = Just "Break Between Exercises:"
            , device = model.device
            }
            data.breakInput
            |> Element.map (UpdateTimeInput BreakIpt)
        , TimeInput.view
            { displayText = Just "Break Between Sets:"
            , device = model.device
            }
            data.setBreakInput
            |> Element.map (UpdateTimeInput SetBreakIpt)

        -- countdown
        , let
            countdownToggle =
                Element.el
                    [ Element.centerY
                    , Element.centerX
                    ]
                <|
                    checkbox
                        { onChange = ToggleCheckbox CountdownCbx
                        , checked = data.countdown
                        , label = Element.text "Countdown:"
                        }

            countdownInput =
                if data.countdown then
                    data.countdownInput
                        |> TimeInput.view
                            { displayText = Nothing
                            , device = model.device
                            }
                        |> Element.map (UpdateTimeInput CountdownIpt)

                else
                    Element.el
                        [ Element.width <| Element.px 188
                        , Element.padding 4
                        ]
                        Element.none
          in
          if Util.isVerticalPhone model.device then
            -- orient vertically
            Element.column
                [ Element.spacing 8
                , Element.centerX
                ]
                [ countdownToggle
                , countdownInput
                ]

          else
            -- orient horizontally (center the space between label and input)
            Element.el
                [ Element.onRight <|
                    Element.el [ Element.centerY ] countdownInput
                , Element.onLeft countdownToggle

                -- HACK (to fix elm-ui bug #243)
                , Element.moveDown 0
                , Element.centerX
                , Element.height (Element.px 64)
                , Element.padding 4
                ]
                Element.none
        ]


viewSounds : Model -> Data.Data -> Element Msg
viewSounds _ data =
    let
        helpIcon =
            Util.viewIcon
                { icon = Icon.helpCircle
                , color = Colours.black
                , size = 20
                , msg = Just SpeechSynthesisToggle
                , withBorder = False
                }
                |> Element.el
                    [ Element.pointer
                    ]

        speechSynthCheckbox =
            checkbox
                { onChange = ToggleCheckbox SpeakCbx
                , checked = data.speak
                , label =
                    Element.row
                        [ Element.spacing 4 ]
                        [ Element.text "Speech Synthesis:"
                        , helpIcon
                        ]
                }
                |> Element.el [ Element.centerX ]

        defaultSoundsCheckbox =
            checkbox
                { onChange = ToggleCheckbox SoundsCbx
                , checked = data.sounds
                , label = Element.text "Default Sounds:"
                }
                |> Element.el [ Element.centerX ]

        toggles =
            Element.column
                [ Element.spacing 8
                , Element.centerX
                ]
                [ speechSynthCheckbox
                , defaultSoundsCheckbox
                ]
    in
    Element.column
        [ Element.centerX
        , Element.spacing 12
        ]
        [ Util.viewIcon
            { icon = Icon.volume2
            , color = Colours.sky
            , size = 40
            , msg = Nothing
            , withBorder = False
            }
            |> Element.el [ Element.centerX ]
        , toggles
        , if not data.sounds && not data.speak then
            Element.paragraph
                [ Font.light
                ]
                [ Element.el [ Font.bold, Font.color Colours.sunset ] <| Element.text "Warning: "
                , Element.text "Your workout will be completely silent."
                ]

          else
            Element.none
        ]


viewSets : Model -> Data.Data -> Element Msg
viewSets model data =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 18
        ]
        [ Util.viewIcon
            { icon = Icon.zap
            , color = Colours.sunflower
            , size = 40
            , msg = Nothing
            , withBorder = False
            }
            |> Element.el [ Element.centerX ]
        , -- total time
          Element.row
            [ Element.spacing 2
            , Element.centerX
            , Font.light
            ]
            [ Element.text "Total time: "
            , Data.totalTime data
                |> Duration.viewFancy
                |> Element.el
                    [ Font.color Colours.sunflower ]
            ]
        , Dict.toList
            data.sets
            |> List.map
                (\( _, set ) ->
                    Lazy.lazy2
                        Set.view
                        { onNewExercise = NewElement

                        -- exercise position then set position
                        , onDeleteExercise = DeleteElement
                        , onDelete = DeleteSet
                        , onUpdateRepeat = NewSetRepeat
                        , sanitizeRepeat = SanitizeRepeat
                        , onCopy = CopySet
                        , toggleExpand = ToggleSetExpand
                        , updateName = UpdateSetName
                        , updateExerciseName = UpdateExerciseName
                        , exerciseDuration = TimeInput.getDuration data.exerciseInput
                        , breakDuration = TimeInput.getDuration data.breakInput
                        , device = model.device
                        }
                        set
                )
            |> List.intersperse (Exercise.breakView <| TimeInput.getDuration data.setBreakInput)
            |> Element.column
                [ Element.spacing 32
                , Element.width Element.fill
                ]

        -- add set
        , Element.el
            [ Element.alignBottom
            , Element.centerX
            ]
            (Util.viewIcon
                { icon = Icon.plus
                , color = Colours.sunflower
                , size = 40
                , msg = Just AddSet
                , withBorder = True
                }
                |> Element.el
                    [ Element.alignBottom
                    , Element.centerX
                    , Background.color Colours.white
                    ]
            )
        ]



-- the popup when the user clicks the (?) on the speech synthesis toggle


speechSynthesisPopup : Model -> Element Msg
speechSynthesisPopup model =
    let
        body =
            Element.textColumn
                [ Element.spacing 16
                , Element.width Element.fill
                , Font.light
                , Font.size 20
                ]
                [ Element.paragraph
                    [ Font.bold
                    , Font.size 32
                    ]
                    [ Element.text "Speech Synthesis" ]
                , Element.paragraph []
                    [ Element.text "An experimental technology that would voice out the exercises and breaks as you work out." ]
                , Element.paragraph
                    []
                    [ Element.el [ Font.bold ] <| Element.text "NOTE: "
                    , Element.text "This will not work on Internet Explorer. "
                    ]
                ]

        closeButton =
            Util.viewIcon
                { icon = Icon.x
                , color = Colours.sunset
                , size = 30
                , msg = Just SpeechSynthesisToggle
                , withBorder = True
                }
                |> Element.el
                    [ Element.centerX ]

        config =
            { closeMessage = Nothing
            , maskAttributes = [ Background.color Colours.transparent ]
            , containerAttributes =
                [ Element.spacing 32
                , Element.centerX
                , Element.centerY
                , Element.width (Element.maximum 600 Element.fill)
                , Element.padding 32
                , Background.color Colours.white
                , Border.width 1
                , Border.color Colours.sky
                , Border.rounded 8
                ]
            , headerAttributes = []
            , bodyAttributes = [ Element.width Element.fill ]
            , footerAttributes = [ Element.width Element.fill ]
            , header = Nothing
            , body = Just body
            , footer = Just closeButton
            }

        dialogConfig =
            if model.speechSynthesisPopup then
                Just config

            else
                Nothing
    in
    Dialog.view dialogConfig



-- my own checkmark


checkbox : { onChange : Bool -> Msg, checked : Bool, label : Element Msg } -> Element Msg
checkbox data =
    Input.checkbox
        [ Font.light
        , Element.padding 4
        ]
        { onChange = data.onChange
        , icon =
            \on ->
                if on then
                    Util.viewIcon
                        { icon = Icon.checkSquare
                        , color = Colours.grass
                        , size = 30
                        , msg = Nothing
                        , withBorder = False
                        }

                else
                    Util.viewIcon
                        { icon = Icon.xSquare
                        , color = Colours.sunset
                        , size = 30
                        , msg = Nothing
                        , withBorder = False
                        }
        , checked = data.checked
        , label =
            Input.labelLeft
                [ Element.padding 8
                , Element.centerY
                , Element.htmlAttribute <| Html.Attributes.class "no-select"
                ]
                data.label
        }



---- UPDATE ----


type Msg
    = NewWindowSize Int Int
    | UpdateTimeInput Input TimeInput.Msg
    | ToggleCheckbox Checkbox Bool
    | NewElement Int
    | DeleteElement Int Int
    | NewSetRepeat Int String
    | SanitizeRepeat Int
    | DeleteSet Int
    | AddSet
    | CopySet Int
    | ToggleSetExpand Int
    | UpdateSetName Int String
    | UpdateExerciseName Int Int String
    | SpeechSynthesisToggle -- opens and closes info popup
    | ToLocalStorage -- save to local storage
    | StoreConfigSuccess -- when local storage succeeds



-- helps me differentiate between the different config stuff


type Input
    = ExerciseIpt
    | BreakIpt
    | SetBreakIpt
    | CountdownIpt


type Checkbox
    = CountdownCbx
    | SpeakCbx
    | SoundsCbx


update : Msg -> Config -> ( Config, Cmd Msg )
update msg (Config model data) =
    case msg of
        NewWindowSize width height ->
            ( Config { model | device = Element.classifyDevice <| Flags.WindowSize width height } data, Cmd.none )

        UpdateTimeInput ExerciseIpt timeInputMsg ->
            let
                ( newInput, save ) =
                    TimeInput.update timeInputMsg data.exerciseInput
            in
            Config model { data | exerciseInput = newInput }
                |> (if save then
                        saveToLocalStorage

                    else
                        \c -> ( c, Cmd.none )
                   )

        UpdateTimeInput BreakIpt timeInputMsg ->
            let
                ( newInput, save ) =
                    TimeInput.update timeInputMsg data.breakInput
            in
            Config model { data | breakInput = newInput }
                |> (if save then
                        saveToLocalStorage

                    else
                        \c -> ( c, Cmd.none )
                   )

        UpdateTimeInput SetBreakIpt timeInputMsg ->
            let
                ( newInput, save ) =
                    TimeInput.update timeInputMsg data.setBreakInput
            in
            Config model { data | setBreakInput = newInput }
                |> (if save then
                        saveToLocalStorage

                    else
                        \c -> ( c, Cmd.none )
                   )

        UpdateTimeInput CountdownIpt timeInputMsg ->
            let
                ( newInput, save ) =
                    TimeInput.update timeInputMsg data.countdownInput
            in
            Config model { data | countdownInput = newInput }
                |> (if save then
                        saveToLocalStorage

                    else
                        \c -> ( c, Cmd.none )
                   )

        ToggleCheckbox CountdownCbx bool ->
            Config model { data | countdown = bool }
                |> saveToLocalStorage

        ToggleCheckbox SpeakCbx bool ->
            Config model { data | speak = bool }
                |> saveToLocalStorage

        ToggleCheckbox SoundsCbx bool ->
            Config model { data | sounds = bool }
                |> saveToLocalStorage

        NewElement setPos ->
            Config model (updateSetDictionary data setPos Set.newExercise)
                |> saveToLocalStorage

        DeleteElement setPos elemPos ->
            Config model (updateSetDictionary data setPos (Set.deleteExercise elemPos >> Set.sanitizeExercises))
                |> saveToLocalStorage

        NewSetRepeat setPos repeat ->
            ( Config model (updateSetDictionary data setPos <| Set.updateRepeatInput repeat)
            , Cmd.none
            )

        SanitizeRepeat setPos ->
            Config model (updateSetDictionary data setPos Set.sanitizeRepeat)
                |> saveToLocalStorage

        DeleteSet setPos ->
            Config model (Data.sanitizeSets { data | sets = Dict.remove setPos data.sets })
                |> saveToLocalStorage

        AddSet ->
            let
                newN =
                    data.setCounter + 1
            in
            Config model
                { data
                    | sets = Dict.insert newN (Set.init newN) data.sets
                    , setCounter = newN
                }
                |> saveToLocalStorage

        CopySet setPos ->
            case Dict.get setPos data.sets of
                Nothing ->
                    ( Config model data, Cmd.none )

                Just setToCopy ->
                    let
                        -- creating a new dict, shifting the keys of the element to make room for the duplicated element
                        newSets =
                            Dict.toList data.sets
                                |> List.map
                                    (\( n, set ) ->
                                        if n > setPos then
                                            ( n + 1, set )

                                        else
                                            ( n, set )
                                    )
                                |> Dict.fromList
                    in
                    Config model
                        { data
                            | sets = Dict.insert (setPos + 1) (Set.updatePosition (setPos + 1) setToCopy) newSets
                            , setCounter = Dict.size data.sets + 1
                        }
                        |> saveToLocalStorage

        ToggleSetExpand setPos ->
            ( Config model (updateSetDictionary data setPos Set.toggleExpand), Cmd.none )

        UpdateSetName setPos newName ->
            Config model (updateSetDictionary data setPos <| Set.updateName newName)
                |> saveToLocalStorage

        UpdateExerciseName setPos exercisePos newName ->
            Config model (updateSetDictionary data setPos <| Set.updateExerciseName exercisePos newName)
                |> saveToLocalStorage

        SpeechSynthesisToggle ->
            ( Config { model | speechSynthesisPopup = not model.speechSynthesisPopup } data, Cmd.none )

        ToLocalStorage ->
            ( Config { model | saving = True } data, Ports.storeConfig (Data.encode data) )

        StoreConfigSuccess ->
            ( Config { model | saving = False } data, Cmd.none )



-- helpers


updateSetDictionary : Data.Data -> Int -> (Set.Set -> Set.Set) -> Data.Data
updateSetDictionary data setPos f =
    { data
        | sets =
            Dict.update
                setPos
                (Maybe.map f)
                data.sets
    }


saveToLocalStorage : Config -> ( Config, Cmd Msg )
saveToLocalStorage config =
    update ToLocalStorage config



---- SUBSCRIPTIONS ----


subscriptions : Config -> Sub Msg
subscriptions (Config model _) =
    let
        newWindowSub =
            if Util.isVerticalPhone model.device then
                -- vertical phones call a new window size event when the keyboard pops up as well, messing up the view function.
                Sub.none

            else
                Browser.Events.onResize NewWindowSize
    in
    Sub.batch
        [ newWindowSub
        , Ports.storeConfigSuccess <| always StoreConfigSuccess
        ]
