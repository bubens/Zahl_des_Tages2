module SimpleTimer exposing (SimpleTimer, State(..), TimerConfiguration, changeColors, create, setDuration, start, stop, tick, toSvg)

import Colors
import Html exposing (Html)
import Svg
import Svg.Attributes as Attr
import Svg.PathD as Path exposing (Segment(..))
import Time exposing (Posix)


type State
    = Idle
    | Running Posix
    | Done


type alias SimpleTimer =
    { duration : Int
    , current : Maybe Posix
    , state : State
    , configuration : TimerConfiguration
    }


type alias TimerConfiguration =
    { timerColor : String
    , borderColor : String
    , borderWidth : Int
    , backgroundColor : String
    }


create : TimerConfiguration -> Int -> SimpleTimer
create configuration durationInMs =
    { duration = durationInMs
    , current = Nothing
    , state = Idle
    , configuration = configuration
    }


setDuration : Int -> SimpleTimer -> SimpleTimer
setDuration duration timer =
    { timer | duration = duration }


changeColors : Colors.ColorScheme -> SimpleTimer -> SimpleTimer
changeColors colors timer =
    let
        oldConfiguration =
            timer.configuration

        newConfiguration =
            { oldConfiguration
                | timerColor = Colors.toHtmlString colors.timer
                , borderColor = Colors.toHtmlString colors.timerBorder
                , backgroundColor = Colors.toHtmlString colors.timerBackground
            }
    in
    { timer
        | configuration = newConfiguration
    }


start : Posix -> SimpleTimer -> SimpleTimer
start now timer =
    case timer.state of
        Running _ ->
            let
                stoppedTimer =
                    stop timer
            in
            { stoppedTimer
                | state = Running now
            }

        _ ->
            { timer
                | state = Running now
            }


stop : SimpleTimer -> SimpleTimer
stop timer =
    { timer
        | state = Idle
        , current = Nothing
    }


tick : Posix -> SimpleTimer -> SimpleTimer
tick now timer =
    case timer.state of
        Running started ->
            let
                runtime =
                    Time.posixToMillis now - Time.posixToMillis started
            in
            if runtime >= timer.duration then
                { timer
                    | state = Done
                    , current = Nothing
                }

            else
                { timer
                    | current = Just now
                }

        _ ->
            timer


toSvg : Int -> SimpleTimer -> Html msg
toSvg width { current, duration, state, configuration } =
    let
        then_ =
            case state of
                Running posix ->
                    Time.posixToMillis posix

                _ ->
                    0

        now =
            current
                |> Maybe.map Time.posixToMillis
                |> Maybe.withDefault 0

        fraction =
            toFloat (now - then_) / toFloat duration

        viewBoxStr w =
            "0 0 " ++ String.fromInt width ++ " " ++ String.fromInt width

        center =
            toFloat width / 2
    in
    Svg.svg
        [ Attr.width <| String.fromInt width
        , Attr.height <| String.fromInt width
        , Attr.viewBox <| viewBoxStr width
        ]
        [ Svg.circle
            [ Attr.cx <| String.fromFloat center
            , Attr.cy <| String.fromFloat center
            , Attr.r <| String.fromFloat (center - 5)
            , Attr.fill configuration.timerColor
            , Attr.strokeWidth (String.fromInt configuration.borderWidth)
            , Attr.stroke configuration.borderColor
            ]
            []
        , Svg.path
            [ Attr.d <| createPath width fraction
            , Attr.stroke configuration.backgroundColor
            , Attr.strokeWidth "0"
            , Attr.fill configuration.backgroundColor
            ]
            []
        ]


createPath : Int -> Float -> String
createPath width fraction =
    let
        fwidth =
            toFloat width

        center =
            fwidth / 2

        radius =
            center

        rotation =
            0

        large =
            if fraction >= 0.5 then
                True

            else
                False

        clockwise =
            True

        phi =
            fraction * (2 * pi) - pi / 2

        target =
            ( radius * cos phi + fwidth / 2
            , radius * sin phi + fwidth / 2
            )
    in
    Path.pathD
        [ M ( center, center )
        , L ( center, 0 )

        --, M target
        , A ( radius, radius ) rotation large clockwise target
        , L ( center, center )
        , Z
        ]
