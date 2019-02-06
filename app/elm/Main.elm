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
    = AddFiveToBench
    | SubtractFiveFromBench
    | AddFiveToSquat
    | SubtractFiveFromSquat
    | AddFiveToDeadlift
    | SubtractFiveFromDeadlift
    | AddFiveToPress
    | SubtractFiveFromPress


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddFiveToBench ->
            { model | bench = model.bench + 5 }

        SubtractFiveFromBench ->
            { model | bench = model.bench - 5 }

        AddFiveToSquat ->
            { model | squat = model.squat + 5 }

        SubtractFiveFromSquat ->
            { model | squat = model.squat - 5 }

        AddFiveToDeadlift ->
            { model | deadlift = model.deadlift + 5 }

        SubtractFiveFromDeadlift ->
            { model | deadlift = model.deadlift - 5 }

        AddFiveToPress ->
            { model | press = model.press + 5 }

        SubtractFiveFromPress ->
            { model | press = model.press - 5 }



-- View


view : Model -> Html Msg
view model =
    div [ class "lift-maxes" ]
        [ liftMaxRow "bench" model.bench AddFiveToBench SubtractFiveFromBench
        , liftMaxRow "squat" model.squat AddFiveToSquat SubtractFiveFromSquat
        , liftMaxRow "deadlift" model.deadlift AddFiveToDeadlift SubtractFiveFromDeadlift
        , liftMaxRow "press" model.press AddFiveToPress SubtractFiveFromPress
        ]


liftMaxRow : String -> Int -> Msg -> Msg -> Html Msg
liftMaxRow name value addFive subtractFive =
    div [ class ("row " ++ name) ]
        [ div [ class "label" ]
            [ text name ]
        , div [ class "value" ]
            [ text (String.fromInt value) ]
        , button [ onClick addFive ] [ text "+5" ]
        , button [ onClick subtractFive ] [ text "-5" ]
        ]
