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
liftGroup lift value =
    div [ class ("group " ++ lift) ]
        [ div [ class "group-header" ] [ text lift ]
        , div [ class "week deload" ]
            [ div [ class "row header" ] [ text "warmup / deload" ]
            , div [ class "row" ] [ text "40% x5" ]
            , div [ class "row" ] [ text "50% x5" ]
            , div [ class "row" ] [ text "60% x5" ]
            ]
        , div [ class "week 555" ]
            [ div [ class "row header" ] [ text "555" ]
            , div [ class "row" ] [ text "65% x5" ]
            , div [ class "row" ] [ text "75% x5" ]
            , div [ class "row" ] [ text "85% x5+" ]
            ]
        , div [ class "week 333" ]
            [ div [ class "row header" ] [ text "333" ]
            , div [ class "row" ] [ text "70% x3" ]
            , div [ class "row" ] [ text "80% x3" ]
            , div [ class "row" ] [ text "90% x3+" ]
            ]
        , div [ class "week 531" ]
            [ div [ class "row header" ] [ text "531" ]
            , div [ class "row" ] [ text "75% x5" ]
            , div [ class "row" ] [ text "85% x3" ]
            , div [ class "row" ] [ text "95% x1+" ]
            ]
        ]
