import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Attr
import Random

-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL

type alias Model =
    { dieFace : Int
    }

init : () -> (Model, Cmd Msg)
init _ =
    (Model 1, Cmd.none)

-- UPDATE

type Msg
    = Roll
    | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )

        NewFace newFace ->
            ( Model newFace
            , Cmd.none
            )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view model =
    div []
        [ h1 [] [ text (String.fromInt model.dieFace)]
        , viewDieFaceImage model
        , newline
        , button [ onClick Roll ] [ text "Roll" ]
        ]

newline : Html Msg
newline =
    br [] []

viewDieFaceImage : Model -> Html Msg
viewDieFaceImage model =
    case model.dieFace of
        1 ->
            img [ Attr.src "image/one.jpg", Attr.height 50, Attr.width 60 ][]
        2 ->
            img [ Attr.src "image/two.jpg", Attr.height 50, Attr.width 60 ][]
        3 ->
            img [ Attr.src "image/three.png", Attr.height 50, Attr.width 60 ][]
        4 ->
            img [ Attr.src "image/four.jpg", Attr.height 50, Attr.width 60 ][]
        5 ->
            img [ Attr.src "image/five.png", Attr.height 50, Attr.width 60 ][]
        _ ->
            img [ Attr.src "image/six.jpeg", Attr.height 50, Attr.width 60 ][]