module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)


{-| This creates the most basic sort of Elm progam available in the
browser. No side effects like HTTP requests are available, just user
input and view rendering. For more options, see the elm/browser package
documentation @ <https://package.elm-lang.org/packages/elm/browser/latest/>
-}
main : Program () Model Msg
main =
    Browser.sandbox
        { init = initalModel
        , update = update
        , view = view
        }



-- Model


type alias Model =
    { bench : Int
    , squat : Int
    , deadlift : Int
    , press : Int
    }


initalModel : Model
initalModel =
    { bench = 0
    , squat = 0
    , deadlift = 0
    , press = 0
    }



-- Update


type Msg
    = AddBench Int
    | AddSquat Int
    | AddDeadlift Int
    | AddPress Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddBench value ->
            { model | bench = model.bench + value }

        AddSquat value ->
            { model | squat = model.squat + value }

        AddDeadlift value ->
            { model | deadlift = model.deadlift + value }

        AddPress value ->
            { model | press = model.press + value }



-- View


view : Model -> Html Msg
view model =
    div [ class "lift-maxes" ]
        [ liftMaxRow "bench" model.bench AddBench
        , liftMaxRow "squat" model.squat AddSquat
        , liftMaxRow "deadlift" model.deadlift AddDeadlift
        , liftMaxRow "press" model.press AddPress
        ]


liftMaxRow : String -> Int -> (Int -> Msg) -> Html Msg
liftMaxRow name value addLift =
    div [ class ("row " ++ name) ]
        [ div [ class "label" ]
            [ text name ]
        , div [ class "value" ]
            [ text (String.fromInt value) ]
        , button [ onClick (addLift 5) ] [ text "+5" ]
        , button [ onClick (addLift -5) ] [ text "-5" ]
        ]
