module Colors exposing (..)


type alias RGBColor =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


type alias ColorScheme =
    { background : RGBColor
    , font : RGBColor
    , fontBackground : RGBColor
    , timer : RGBColor
    , timerBorder : RGBColor
    , timerBackground : RGBColor
    , feedbackWrong : RGBColor
    , feedbackCorrect : RGBColor
    , name : String
    }


toHtmlString : RGBColor -> String
toHtmlString rgb =
    "rgba("
        ++ String.fromInt rgb.red
        ++ ", "
        ++ String.fromInt rgb.green
        ++ ", "
        ++ String.fromInt rgb.blue
        ++ ", "
        ++ String.fromFloat rgb.alpha
        ++ ")"


setOpacity : Float -> RGBColor -> RGBColor
setOpacity rate color =
    { color | alpha = rate }


lightMode : ColorScheme
lightMode =
    { background = RGBColor 228 223 218 1
    , font = RGBColor 39 71 78 1
    , fontBackground = RGBColor 134 147 148 1
    , timer = RGBColor 196 91 170 1
    , timerBorder = RGBColor 125 56 125 1
    , timerBackground = RGBColor 228 180 216 1
    , feedbackWrong = RGBColor 237 118 128 1
    , feedbackCorrect = RGBColor 118 237 146 1
    , name = "lightMode"
    }


darkMode : ColorScheme
darkMode =
    { background = RGBColor 28 30 32 1
    , font = RGBColor 220 226 228 1
    , fontBackground = RGBColor 55 65 68 1
    , timer = RGBColor 186 104 170 1
    , timerBorder = RGBColor 120 70 130 1
    , timerBackground = RGBColor 48 38 48 1
    , feedbackWrong = RGBColor 237 118 128 1
    , feedbackCorrect = RGBColor 118 237 146 1
    , name = "darkMode"
    }
