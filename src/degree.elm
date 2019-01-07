import Browser
import Html exposing (Html, Attribute, span, input, text, div, br )
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { celsiusInput : String
  , fahreniteInput : String
  }


init : Model
init =
  Model "" ""



-- UPDATE


type Msg
  = ChangeToCelsius String
  | ChangeToFahrenite String


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeToCelsius newInput ->
      { model | fahreniteInput = newInput }

    ChangeToFahrenite newInput ->
      { model |  celsiusInput = newInput }




-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewConvertCelsiusToFahrenite model
    , newline
    , newline
    , viewConvertFahreniteToCelsius model
    ]

newline : Html Msg
newline = br [] []

viewConvertCelsiusToFahrenite : Model -> Html Msg
viewConvertCelsiusToFahrenite model =
    case String.toFloat model.celsiusInput of
    Just celsius ->
      viewConverter "cTof" model.celsiusInput "blue" (String.fromFloat (celsius * 1.8 + 32)) "°C" "°F "

    Nothing ->
      viewConverter "cTof" model.celsiusInput "red" "???" "°C " "°F "

    
viewConvertFahreniteToCelsius : Model -> Html Msg
viewConvertFahreniteToCelsius model =
    case String.toFloat model.fahreniteInput of
        Just fahrenite ->
            viewConverter "fToc" model.fahreniteInput "blue" (String.fromFloat ((fahrenite - 32) / 1.8)) "°F" "°C"

        Nothing ->
            viewConverter "fToc" model.fahreniteInput "red" "???" "°F " "°C = "


viewConverter : String-> String -> String -> String -> String -> String -> Html Msg
viewConverter name userInput color equivalentTemp sourceTemp destTemp =
    
    
  span []
    [ viewInput name userInput color
    , text sourceTemp 
    , span [ style "color" color ] [ text equivalentTemp ]
    , text destTemp
    ]

viewInput : String -> String -> String -> Html Msg
viewInput name userInput color =
  let
    bgColor =
        case color of
            "red" ->
                case String.length userInput of
                    0 ->
                        ""
                    _ ->
                        "red"
            _ ->
                ""
  in
  case name of 
    "fToc" -> 
      input [ value userInput, id name, onInput ChangeToCelsius, style "width" "40px", style "border-color" bgColor ] []
    
    _ -> 
      input [ value userInput, id name, onInput ChangeToFahrenite, style "width" "40px", style "border-color" bgColor ] []
