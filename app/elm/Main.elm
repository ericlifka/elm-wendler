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
    }


initalModel : Model
initalModel =
    { bench = 0
    , squat = 0
    }



-- Update


type Msg
    = AddFiveToBench
    | SubtractFiveFromBench
    | AddFiveToSquat
    | SubtractFiveFromSquat


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



-- View


view : Model -> Html Msg
view model =
    div [ class "lift-maxes" ]
        [ div [ class "row bench" ]
            [ div [ class "label" ]
                [ text "Bench" ]
            , div [ class "value" ]
                [ text (String.fromInt model.bench) ]
            , button [ onClick AddFiveToBench ] [ text "+5" ]
            , button [ onClick SubtractFiveFromBench ] [ text "-5" ]
            ]
        , div [ class "row squat" ]
            [ div [ class "label" ]
                [ text "Squat" ]
            , div [ class "value" ]
                [ text (String.fromInt model.squat) ]
            , button [ onClick AddFiveToSquat ] [ text "+5" ]
            , button [ onClick SubtractFiveFromSquat ] [ text "-5" ]
            ]
        ]
