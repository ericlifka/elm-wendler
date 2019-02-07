module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Round


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
    { bench = 65
    , squat = 85
    , deadlift = 110
    , press = 45
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
    div []
        [ div [ class "lift-maxes" ]
            [ liftMaxRow "Bench" model.bench AddBench
            , liftMaxRow "Squat" model.squat AddSquat
            , liftMaxRow "Deadlift" model.deadlift AddDeadlift
            , liftMaxRow "Press" model.press AddPress
            ]
        , div [ class "lift-groups" ]
            [ liftGroup "Bench" model.bench
            , liftGroup "Squat" model.squat
            , liftGroup "Deadlift" model.deadlift
            , liftGroup "Press" model.press
            ]
        ]


liftMaxRow : String -> Int -> (Int -> Msg) -> Html Msg
liftMaxRow lift value addLift =
    div [ class ("row " ++ lift) ]
        [ div [ class "label" ]
            [ text lift ]
        , div [ class "value" ]
            [ text (String.fromInt value) ]
        , button [ onClick (addLift 5) ] [ text "+5" ]
        , button [ onClick (addLift -5) ] [ text "-5" ]
        ]


liftGroup : String -> Int -> Html Msg
liftGroup lift max =
    div [ class ("group " ++ lift) ]
        [ button [ class "group-header header" ] [ text lift ]
        , div [ class "week deload" ]
            [ button [ class "row header" ] [ text "warmup / deload" ]
            , div [ class "row" ] [ calcWeightTarget max 0.4 "5" ]
            , div [ class "row" ] [ calcWeightTarget max 0.5 "5" ]
            , div [ class "row" ] [ calcWeightTarget max 0.6 "5" ]
            ]
        , div [ class "week 555" ]
            [ button [ class "row header" ] [ text "555" ]
            , div [ class "row" ] [ calcWeightTarget max 0.65 "5" ]
            , div [ class "row" ] [ calcWeightTarget max 0.75 "5" ]
            , div [ class "row" ] [ calcWeightTarget max 0.85 "5+" ]
            ]
        , div [ class "week 333" ]
            [ button [ class "row header" ] [ text "333" ]
            , div [ class "row" ] [ calcWeightTarget max 0.7 "3" ]
            , div [ class "row" ] [ calcWeightTarget max 0.8 "3" ]
            , div [ class "row" ] [ calcWeightTarget max 0.9 "3+" ]
            ]
        , div [ class "week 531" ]
            [ button [ class "row header" ] [ text "531" ]
            , div [ class "row" ] [ calcWeightTarget max 0.75 "5" ]
            , div [ class "row" ] [ calcWeightTarget max 0.85 "3" ]
            , div [ class "row" ] [ calcWeightTarget max 0.95 "1+" ]
            ]
        ]


calcWeightTarget : Int -> Float -> String -> Html Msg
calcWeightTarget max percent count =
    text (String.fromFloat (percent * toFloat max) ++ " lbs x" ++ count)
