import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import String exposing (any)
import Char exposing (isUpper, isDigit, isLower)
import List exposing (filterMap, map, isEmpty, foldl)
import Debug
main =
    Browser.sandbox{ init = init, update = update, view = view }

-- MODEL

type alias ValidationError = String

type alias ValidationErrors = List ValidationError

type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , errors : Maybe (ValidationErrors)
    }

init : Model
init =
    { name= ""
    , password= "" 
    , passwordAgain= ""
    , age = ""
    , errors = Nothing
    }

-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Validate

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }
        
        Password password ->
            { model | password = password }
        
        PasswordAgain password ->
            { model | passwordAgain = password }
    
        Age value ->
            { model | age = value}
        
        Validate ->
            { model | errors = Just (validate model) }
    
-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ viewForm model
        , viewResult model
        ]

viewForm : Model -> Html Msg
viewForm model =
  Html.form [ onSubmit Validate ]
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , newline
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , newline
    , input [ type_ "password"
            , placeholder "Re-enter Password"
            , onInput PasswordAgain ] []
    , newline
    , input [ type_ "text", placeholder "Age", onInput Age ] []
    , newline
    , input [ type_ "Submit" ] []
    ]

newline = br [] []

viewResult : Model -> Html a
viewResult model =
    case model.errors of
        Nothing ->
            text ""

        Just errors ->
            if isEmpty errors then
                viewSuccess
            else
                viewErrors errors

viewSuccess =
    div [ style "color" "green" ] [ text "OK"]

viewErrors : ValidationErrors -> Html msg
viewErrors errs =
    ul [ class "errors"] (map viewError errs)

viewError : ValidationError -> Html msg
viewError err =
    li [ style "color" "red"] [text err]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []

validatePasswordLength : Model -> Maybe ValidationError
validatePasswordLength model =
    let
        pwLen = String.length model.password
    in
        if pwLen >= 8 then
            Nothing
        else if pwLen == 0 then
            Just "Password can not be empty"
        else 
            Just "Password is too short"
    
validatePasswordConfirm : Model -> Maybe ValidationError
validatePasswordConfirm model =
    if model.password == model.password then
        Nothing
    else
        Just "Passwords do not match"

validateAgeShouldBeNumber: Model -> Maybe ValidationError
validateAgeShouldBeNumber model =
    case String.toInt model.age of
        Just age ->
            Nothing
        Nothing ->
            Just "Age should be a number"

validatePasswordDifficulty: Model -> Maybe ValidationError
validatePasswordDifficulty model =
    if allPass [any isDigit, any isUpper, any isLower] model.password then
        Nothing
    else
        Just "Password should contain upper case, lower case, and numeric characters"

allPass : List(a-> Bool) -> a -> Bool
allPass predicates arg1 =
    foldl (\pred acc -> (pred arg1) && acc) True predicates

validate : Model -> ValidationErrors
validate model =
    filterMap
        (\validator -> validator model)
        [ validatePasswordLength
        , validatePasswordConfirm
        , validateAgeShouldBeNumber
        , validatePasswordDifficulty
        ]


