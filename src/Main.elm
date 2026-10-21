port module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Colors
import Delay
import Element as Elm exposing (Element, centerX, centerY, column, el, explain, fill, fillPortion, height, px, row, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Icons
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Random
import SimpleTimer exposing (SimpleTimer)
import Task
import Time exposing (Posix)
import Wiggle



-- TYPES


type alias Directions =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


type alias Flags =
    { minimum : Value String
    , maximum : Value String
    , workingTime : Value String
    , separator : Char
    , currentColorScheme : Colors.ColorScheme
    , settingsOpen : Bool
    , windowWidth : Float
    , windowHeight : Float
    }


type State
    = Idle
    | Rolling
    | Working
    | Done


type Value a
    = Valid a
    | Invalid a


type alias Settings =
    { minimum : Value String
    , maximum : Value String
    , workingTime : Value String
    , separator : Char
    }


type alias WindowSizes =
    { width : Float
    , height : Float
    }


type SettingsInputData
    = Minimum String
    | Maximum String
    | WorkingTime String


type alias Model =
    { number : Maybe Int
    , range : Maybe ( Int, Int )
    , currentColorScheme : Colors.ColorScheme
    , state : State
    , timer : SimpleTimer
    , mouseOverSettingsIcon : Bool
    , settingsOpen : Bool
    , settings : Settings
    , windowWidth : Float
    , windowHeight : Float
    , isFullscreen : Bool
    }


type Msg
    = DoNothing
    | GenerateNextNumber
    | NextRandomNumber Int
    | DoneRolling
    | StartRolling
    | StartTimer Posix
    | TickTimer Posix
    | MouseOverSettingsIcon Bool
    | ToggleSettings
    | InputSettings SettingsInputData
    | AudioEventDone String
    | SetColorScheme Colors.ColorScheme
    | SetSeparator Char
    | WindowResize WindowSizes
    | ToggleFullscreen
    | EnterFullscreen
    | ExitFullscreen



-- DATA


delays : List Int
delays =
    [ 10
    , 10
    , 10
    , 10
    , 10
    , 30
    , 20
    , 30
    , 30
    , 50
    , 50
    , 50
    , 80
    , 80
    , 80
    , 100
    , 100
    , 100
    , 150
    , 150
    , 150
    , 300
    , 300
    , 300
    , 500
    , 500
    , 500
    , 800
    , 1000
    ]



-- Decoder


charifyWithDefault : Char -> String -> Char
charifyWithDefault default str =
    String.uncons str
        |> Maybe.withDefault ( default, "blablabla" )
        |> Tuple.first


getColorScheme : String -> Colors.ColorScheme
getColorScheme name =
    case name of
        "darkMode" ->
            Colors.darkMode

        _ ->
            Colors.lightMode


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> Decode.required "minimum" (Decode.map checkInput Decode.string)
        |> Decode.required "maximum" (Decode.map checkInput Decode.string)
        |> Decode.required "workingTime" (Decode.map checkInput Decode.string)
        |> Decode.required "separator" (Decode.map (charifyWithDefault '.') Decode.string)
        |> Decode.required "currentColorScheme" (Decode.map getColorScheme Decode.string)
        |> Decode.required "settingsOpen" Decode.bool
        |> Decode.required "windowWidth" Decode.float
        |> Decode.required "windowHeight" Decode.float


windowSizeDecoder : Decoder WindowSizes
windowSizeDecoder =
    Decode.map2 WindowSizes
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)



-- Encoder


stringifyValue : Value String -> String
stringifyValue val =
    case val of
        Valid str ->
            str

        Invalid str ->
            str


settingsEncoder : Model -> Encode.Value
settingsEncoder model =
    Encode.object
        [ ( "minimum", Encode.string (stringifyValue model.settings.minimum) )
        , ( "maximum", Encode.string (stringifyValue model.settings.maximum) )
        , ( "workingTime", Encode.string (stringifyValue model.settings.workingTime) )
        , ( "separator", Encode.string (String.fromChar model.settings.separator) )
        , ( "currentColorScheme", Encode.string model.currentColorScheme.name )
        , ( "settingsOpen", Encode.bool model.settingsOpen )
        ]



-- PORTS
-- OUTGOING


port triggerAudioEvent : String -> Cmd msg


port storeSettings : Encode.Value -> Cmd msg


port toggleFullscreen : Int -> Cmd msg



-- INCOMING


port audioEventDone : (String -> msg) -> Sub msg


port windowResized : (Decode.Value -> msg) -> Sub msg


port fullscreenChange : (String -> msg) -> Sub msg



-- RANDOM


generateNumber : ( Int, Int ) -> Random.Generator Int
generateNumber ( min, max ) =
    Random.int min max



-- INIT


initTimer : Colors.ColorScheme -> Int -> SimpleTimer.SimpleTimer
initTimer colors duration =
    let
        configuration =
            SimpleTimer.TimerConfiguration
                (Colors.toHtmlString colors.timer)
                (Colors.toHtmlString colors.timerBorder)
                7
                (Colors.toHtmlString colors.timerBackground)
    in
    SimpleTimer.create configuration (duration * 1000)


decodeFlags : Decode.Value -> Flags
decodeFlags json =
    let
        decodedData =
            Decode.decodeValue flagsDecoder json
    in
    case decodedData of
        Ok flags ->
            flags

        Err err ->
            {- let
                   test =
                       Debug.log "Fehler: " err
               in
            -}
            Flags
                (Valid "0")
                (Valid "999")
                (Valid "180")
                '.'
                Colors.lightMode
                False
                1200
                1000


init : Decode.Value -> ( Model, Cmd msg )
init flags =
    let
        decodedFlags =
            decodeFlags
                flags
    in
    ( { number = Nothing
      , range = Nothing
      , state = Idle
      , currentColorScheme = decodedFlags.currentColorScheme
      , timer = initTimer decodedFlags.currentColorScheme 10
      , mouseOverSettingsIcon = False
      , settingsOpen = decodedFlags.settingsOpen
      , settings =
            { minimum = decodedFlags.minimum
            , maximum = decodedFlags.maximum
            , workingTime = decodedFlags.workingTime
            , separator = decodedFlags.separator
            }
      , windowWidth = decodedFlags.windowWidth
      , windowHeight = decodedFlags.windowHeight
      , isFullscreen = False
      }
    , Cmd.none
    )



-- UPDATE


checkInput : String -> Value String
checkInput input =
    String.toInt input
        |> Maybe.map (\_ -> Valid input)
        |> Maybe.withDefault (Invalid input)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        GenerateNextNumber ->
            case model.range of
                Just range ->
                    ( model, Random.generate NextRandomNumber (generateNumber range) )

                Nothing ->
                    ( model, Cmd.none )

        NextRandomNumber x ->
            case model.state of
                Rolling ->
                    ( { model | number = Just x }, triggerAudioEvent "click" )

                _ ->
                    ( model, Cmd.none )

        StartRolling ->
            case model.state of
                Rolling ->
                    ( model, Cmd.none )

                Working ->
                    ( model, Cmd.none )

                _ ->
                    let
                        sequence =
                            List.indexedMap
                                (\i v ->
                                    if i + 1 == List.length delays then
                                        ( v, DoneRolling )

                                    else
                                        ( v, GenerateNextNumber )
                                )
                                delays

                        { settings } =
                            model

                        curRange =
                            case ( settings.minimum, settings.maximum ) of
                                ( Valid x, Valid y ) ->
                                    Just
                                        ( String.toInt x |> Maybe.withDefault 0
                                        , String.toInt y |> Maybe.withDefault 999999
                                        )

                                ( _, _ ) ->
                                    Nothing
                    in
                    case curRange of
                        Just range ->
                            ( { model | state = Rolling, range = curRange }, Delay.sequence sequence )

                        Nothing ->
                            if not model.settingsOpen then
                                ( model, Task.perform identity (Task.succeed ToggleSettings) )

                            else
                                ( model, Cmd.none )

        StartTimer now ->
            let
                timespan =
                    case model.settings.workingTime of
                        Valid t ->
                            String.toInt t
                                |> Maybe.map ((*) 1000)
                                |> Maybe.withDefault model.timer.duration

                        Invalid t ->
                            model.timer.duration
            in
            ( { model
                | timer =
                    SimpleTimer.setDuration timespan model.timer
                        |> SimpleTimer.start now
              }
            , triggerAudioEvent "rollingDone"
            )

        DoneRolling ->
            ( { model | state = Working }
            , Task.perform StartTimer Time.now
            )

        TickTimer now ->
            let
                state =
                    .state model.timer
            in
            case state of
                SimpleTimer.Running _ ->
                    ( { model | timer = SimpleTimer.tick now model.timer }, Cmd.none )

                SimpleTimer.Done ->
                    ( { model | state = Done }, triggerAudioEvent "timerDone" )

                _ ->
                    ( model, Cmd.none )

        MouseOverSettingsIcon state ->
            ( { model | mouseOverSettingsIcon = state }, Cmd.none )

        ToggleSettings ->
            {- let
                   newModel =
                       case model.state of
                           Idle ->
                               { model | settingsOpen = not model.settingsOpen }

                           Done ->
                               { model | settingsOpen = not model.settingsOpen }

                           _ ->
                               model
               in
               ( newModel, Cmd.none )
            -}
            let
                newModel =
                    { model | settingsOpen = not model.settingsOpen }
            in
            ( newModel
            , storeSettings (settingsEncoder newModel)
            )

        InputSettings value ->
            let
                { settings } =
                    model

                newSettings =
                    case value of
                        Maximum str ->
                            { settings | maximum = checkInput str }

                        Minimum str ->
                            { settings | minimum = checkInput str }

                        WorkingTime str ->
                            { settings | workingTime = checkInput str }

                newModel =
                    { model | settings = newSettings }
            in
            ( newModel
            , storeSettings (settingsEncoder newModel)
            )

        AudioEventDone audioEvent ->
            case audioEvent of
                "timerDone" ->
                    ( { model | state = Idle, number = Nothing, range = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetColorScheme scheme ->
            let
                newTimer =
                    SimpleTimer.changeColors scheme model.timer

                newModel =
                    { model | currentColorScheme = scheme, timer = newTimer }
            in
            ( newModel
            , storeSettings (settingsEncoder newModel)
            )

        SetSeparator str ->
            let
                { settings } =
                    model

                newSettings =
                    { settings | separator = str }

                newModel =
                    { model | settings = newSettings }
            in
            ( newModel
            , storeSettings (settingsEncoder newModel)
            )

        WindowResize newSizes ->
            ( { model
                | windowHeight = newSizes.height
                , windowWidth = newSizes.width
              }
            , Cmd.none
            )

        ToggleFullscreen ->
            ( model, toggleFullscreen 67 )

        EnterFullscreen ->
            ( { model | isFullscreen = True }, Cmd.none )

        ExitFullscreen ->
            ( { model | isFullscreen = False }, Cmd.none )


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    { title = "Zahl des Tages"
    , body = [ mainView model ]
    }


insertSeparator : Char -> Int -> String
insertSeparator separator x =
    let
        isNegative =
            x < 0

        n =
            if isNegative then
                x * -1

            else
                x

        s =
            String.fromInt n
                |> (List.reverse << String.toList)
                |> List.indexedMap
                    (\i v ->
                        if i /= 0 && remainderBy 3 i == 0 then
                            [ separator, v ]

                        else
                            [ v ]
                    )
                |> (String.fromList << List.reverse << List.concat)
    in
    if isNegative then
        "-" ++ s

    else
        s


formatNumber : Char -> String -> Int -> String
formatNumber separator max x =
    let
        len =
            String.length max
    in
    insertSeparator separator x
        |> String.padLeft len ' '


viewTimer : Model -> Element Msg
viewTimer { state, timer, windowHeight } =
    let
        height =
            round (0.7 * windowHeight)
    in
    case state of
        Working ->
            SimpleTimer.toSvg height timer
                |> Elm.html
                |> Elm.el [ centerX, centerY ]

        _ ->
            Elm.none


viewValue : Colors.ColorScheme -> Value String -> ( String, Elm.Attr decorative msg, Elm.Attr decorative msg )
viewValue colors value =
    case value of
        Valid v ->
            ( v
            , Border.glow (Elm.fromRgb255 colors.feedbackCorrect) 0
            , Border.color (Elm.fromRgb255 colors.font)
            )

        Invalid v ->
            ( v
            , Border.glow (Elm.fromRgb255 colors.feedbackWrong) 5
            , Border.color (Elm.fromRgb255 colors.feedbackWrong)
            )


viewSeparatorOption : String -> String -> Element msg
viewSeparatorOption example explanation =
    Elm.row
        [ Elm.spacing 4 ]
        [ Elm.column
            [ Font.bold ]
            [ Elm.text example ]
        , Elm.column
            []
            [ Elm.text explanation ]
        ]


viewSettings : Model -> Element Msg
viewSettings model =
    if model.settingsOpen then
        let
            { settings } =
                model

            viewValueWithColor =
                viewValue model.currentColorScheme

            ( minimum, minimumGlow, minimumBorder ) =
                viewValueWithColor settings.minimum

            ( maximum, maximumGlow, maximumBorder ) =
                viewValueWithColor settings.maximum

            ( workingTime, workingTimeGlow, workingTimeBorder ) =
                viewValueWithColor settings.workingTime

            h3Attributes =
                [ Region.heading 3
                , Font.size 20
                , Font.regular
                , Elm.paddingEach (Directions 23 0 5 23)
                , width fill
                ]

            rowAttributes =
                [ Font.size 17
                , Font.light
                , Elm.spacing 8
                , centerX
                ]

            inputAttributes glow border =
                [ glow
                , border
                , width (Elm.px 100)
                , Border.solid
                , Border.width 2
                , Elm.padding 6
                , Font.center
                , Font.size 12
                , Background.color (Elm.fromRgb255 model.currentColorScheme.background)
                , Font.family [ Font.typeface "Martian Mono", Font.monospace ]
                ]
        in
        column
            [ width (px 400)
            , height (px 600)
            , centerX
            , centerY
            , Background.color (Elm.fromRgb255 model.currentColorScheme.fontBackground)
            , Border.width 5
            , Border.color (Elm.fromRgb255 model.currentColorScheme.font)
            , Border.rounded 15
            , Font.family [ Font.typeface "IBM Plex Sans", Font.sansSerif ]
            , Font.color (Elm.fromRgb255 model.currentColorScheme.font)
            ]
            [ Elm.row
                [ Region.heading 2
                , Font.size 24
                , Font.underline
                , centerX
                , Font.medium
                , Elm.paddingEach (Directions 23 0 0 0)
                ]
                [ Elm.text "Einstellungen" ]
            , Elm.row
                h3Attributes
                [ Elm.text "Zahlenbereich:" ]
            , Elm.row
                rowAttributes
                [ Elm.text "von"
                , Input.text
                    (inputAttributes
                        minimumGlow
                        minimumBorder
                    )
                    { onChange = \s -> InputSettings (Minimum s)
                    , text = minimum
                    , placeholder = Nothing
                    , label = Input.labelHidden "Minimum"
                    }
                , Elm.text "bis"
                , Input.text
                    (inputAttributes
                        maximumGlow
                        maximumBorder
                    )
                    { onChange = \s -> InputSettings (Maximum s)
                    , text = maximum
                    , placeholder = Nothing
                    , label = Input.labelHidden "Maximum"
                    }
                ]
            , Elm.row
                h3Attributes
                [ Elm.text "Trennzeichen:" ]
            , Elm.row
                rowAttributes
                [ Input.radio
                    [ Font.size 13
                    , Elm.spacing 5
                    ]
                    { onChange = SetSeparator
                    , options =
                        [ Input.option '.' (viewSeparatorOption "12.345" "(Punkt)")
                        , Input.option ',' (viewSeparatorOption "12,345" "(Komma)")
                        , Input.option '\'' (viewSeparatorOption "12'345" "(Hochstellkomma)")
                        , Input.option ' ' (viewSeparatorOption "12 345" "(Leerzeichen)")
                        , Input.option '\u{200A}' (viewSeparatorOption "12\u{200A}345" "(Halbes Leerzeichen)")
                        , Input.option '\u{200E}' (viewSeparatorOption "12345" "(Kein Trennzeichen)")
                        ]
                    , selected = Just model.settings.separator
                    , label = Input.labelHidden "Trennzeichen"
                    }
                ]
            , Elm.row
                h3Attributes
                [ Elm.text "Arbeitszeit:" ]
            , Elm.row
                rowAttributes
                [ Input.text
                    (inputAttributes
                        workingTimeGlow
                        workingTimeBorder
                    )
                    { onChange = \s -> InputSettings (WorkingTime s)
                    , text = workingTime
                    , placeholder = Nothing
                    , label = Input.labelHidden "Arbeitszeit"
                    }
                , Elm.text "Sekunden"
                ]
            , Elm.row
                h3Attributes
                [ Elm.text "Farbmodus: " ]
            , Elm.row
                rowAttributes
                [ Input.radio
                    []
                    { onChange = SetColorScheme
                    , options =
                        [ Input.option Colors.darkMode (Elm.text "Dark Mode")
                        , Input.option Colors.lightMode (Elm.text "Light Mode")
                        ]
                    , selected = Just model.currentColorScheme
                    , label = Input.labelHidden "Farbschema"
                    }
                ]
            ]

    else
        Elm.none


viewSettingsIcon : Model -> Float -> Element Msg
viewSettingsIcon model width =
    let
        wiggle =
            if model.mouseOverSettingsIcon then
                Wiggle.apply "wiggle-cogwheel" 400
                    |> Elm.htmlAttribute

            else
                Font.unitalicized
    in
    el
        [ Elm.alignRight
        , Elm.pointer
        , Events.onMouseEnter (MouseOverSettingsIcon True)
        , Events.onMouseLeave (MouseOverSettingsIcon False)
        , Events.onClick ToggleSettings
        ]
        (el
            [ wiggle
            , centerX
            , centerY
            , Elm.htmlAttribute (Html.Attributes.title "Einstellungen!")
            ]
            (Elm.html
                (Icons.render
                    Icons.Settings
                    model.currentColorScheme.fontBackground
                    model.currentColorScheme.font
                    width
                )
            )
        )


viewGoIcon : Model -> Float -> Element Msg
viewGoIcon model width =
    el
        [ Elm.alignRight
        , Elm.pointer
        , Events.onClick StartRolling
        ]
        (el
            [ centerX
            , centerY
            , Elm.htmlAttribute (Html.Attributes.title "Go!")
            ]
            (Elm.html
                (Icons.render
                    Icons.Go
                    model.currentColorScheme.font
                    model.currentColorScheme.fontBackground
                    width
                )
            )
        )


viewFullscreenIcon : Model -> Float -> Element Msg
viewFullscreenIcon model width =
    let
        icon =
            if model.isFullscreen then
                Icons.ExitFullscreen

            else
                Icons.EnterFullscreen

        titleText =
            if model.isFullscreen then
                "Vollbild verlassen"

            else
                "Vollbild"
    in
    el
        [ Elm.alignRight
        , Elm.pointer
        , Events.onClick ToggleFullscreen
        , Elm.centerX
        , Elm.centerY
        , Elm.htmlAttribute (Html.Attributes.title titleText)
        ]
        (Elm.html
            (Icons.render
                icon
                model.currentColorScheme.font
                model.currentColorScheme.fontBackground
                width
            )
        )


mainView : Model -> Html Msg
mainView model =
    let
        x =
            case model.number of
                Nothing ->
                    "bereit?"

                Just number ->
                    case model.settings.maximum of
                        Valid max ->
                            formatNumber model.settings.separator max number

                        Invalid _ ->
                            "Fehler (21)"

        fontSize =
            round (0.2 * model.windowHeight)

        additionalAttributes =
            case model.state of
                Working ->
                    let
                        color =
                            Elm.fromRgb255 model.currentColorScheme.font

                        backgroundColor =
                            model.currentColorScheme.fontBackground
                                |> Colors.setOpacity 0.5
                                |> Elm.fromRgb255
                    in
                    [ Font.color color
                    , Background.color backgroundColor
                    , Border.rounded 30
                    , Border.color color
                    , Border.width 4
                    , Border.glow backgroundColor 4
                    ]

                _ ->
                    [ Font.color (Elm.fromRgb255 model.currentColorScheme.font) ]
    in
    Elm.layout
        [ Background.color (Elm.fromRgb255 model.currentColorScheme.background)
        ]
        (column
            [ height fill
            , width fill
            , centerX
            , centerY
            ]
            [ -- css for the wiggle animation
              Wiggle.setUp "wiggle-cogwheel" [ 33, -33 ]
                |> Elm.html
            , row
                [ width fill
                , height (fillPortion 95)
                , Elm.behindContent (viewTimer model)
                , Elm.inFront (viewSettings model)
                ]
                [ el
                    (List.append
                        [ centerX
                        , centerY
                        , Font.size fontSize
                        , Elm.padding 80
                        , Font.family
                            [ Font.typeface "Martian Mono"
                            , Font.monospace
                            ]
                        ]
                        additionalAttributes
                    )
                    (text x)
                ]
            , row
                [ width fill
                , height (fillPortion 3)
                , Elm.padding 20
                , Elm.spacing 15
                ]
                [ viewGoIcon model 35
                , viewSettingsIcon model 35
                , viewFullscreenIcon model 35
                ]
            ]
        )



-- SUBSCRIPTIONS


handleFullscreenChange : String -> Msg
handleFullscreenChange value =
    case value of
        "enter" ->
            EnterFullscreen

        _ ->
            ExitFullscreen


handleWindowResize : Decode.Value -> Msg
handleWindowResize value =
    let
        newSizes =
            Decode.decodeValue windowSizeDecoder value
    in
    case newSizes of
        Ok sizes ->
            WindowResize sizes

        Err _ ->
            DoNothing


subscriptions : Model -> Sub Msg
subscriptions { state } =
    let
        timerTick =
            case state of
                Working ->
                    onAnimationFrame TickTimer

                _ ->
                    Sub.none
    in
    Sub.batch
        [ timerTick
        , audioEventDone AudioEventDone
        , windowResized handleWindowResize
        , fullscreenChange handleFullscreenChange
        ]


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
