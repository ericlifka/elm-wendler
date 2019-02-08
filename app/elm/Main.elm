module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList, id)
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
    , bar : Float
    , plates : List Float
    , benchVisible : Bool
    , squatVisible : Bool
    , deadliftVisible : Bool
    , pressVisible : Bool
    }


initalModel : Model
initalModel =
    { bench = 65
    , squat = 85
    , deadlift = 110
    , press = 45
    , bar = 45
    , plates = [ 45, 35, 25, 10, 5, 2.5 ]
    , benchVisible = False
    , squatVisible = False
    , deadliftVisible = False
    , pressVisible = False
    }



-- Update


type Msg
    = AddBench Int
    | AddSquat Int
    | AddDeadlift Int
    | AddPress Int
    | ToggleBench
    | ToggleSquat
    | ToggleDeadlift
    | TogglePress


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

        ToggleBench ->
            { model
                | benchVisible = not model.benchVisible
                , squatVisible = False
                , deadliftVisible = False
                , pressVisible = False
            }

        ToggleSquat ->
            { model
                | benchVisible = False
                , squatVisible = not model.squatVisible
                , deadliftVisible = False
                , pressVisible = False
            }

        ToggleDeadlift ->
            { model
                | benchVisible = False
                , squatVisible = False
                , deadliftVisible = not model.deadliftVisible
                , pressVisible = False
            }

        TogglePress ->
            { model
                | benchVisible = False
                , squatVisible = False
                , deadliftVisible = False
                , pressVisible = not model.pressVisible
            }



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
            [ liftGroup model "Bench" model.bench model.benchVisible ToggleBench
            , liftGroup model "Squat" model.squat model.squatVisible ToggleSquat
            , liftGroup model "Deadlift" model.deadlift model.deadliftVisible ToggleDeadlift
            , liftGroup model "Press" model.press model.pressVisible TogglePress
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


liftGroup : Model -> String -> Int -> Bool -> Msg -> Html Msg
liftGroup model lift max visible toggleVisible =
    div
        [ classList
            [ ( "group", True )
            , ( lift, True )
            , ( "visible", visible )
            ]
        ]
        [ button
            [ onClick toggleVisible
            , class "group-header header"
            ]
            [ text lift ]
        , div [ class "week deload" ]
            [ button [ class "row header" ] [ text "warmup / deload" ]
            , createLiftTargetRow model max 0.4 "5"
            , createLiftTargetRow model max 0.5 "5"
            , createLiftTargetRow model max 0.6 "5"
            ]
        , div [ class "week 555" ]
            [ button [ class "row header" ] [ text "5-5-5" ]
            , createLiftTargetRow model max 0.65 "5"
            , createLiftTargetRow model max 0.75 "5"
            , createLiftTargetRow model max 0.85 "5+"
            ]
        , div [ class "week 333" ]
            [ button [ class "row header" ] [ text "3-3-3" ]
            , createLiftTargetRow model max 0.7 "3"
            , createLiftTargetRow model max 0.8 "3"
            , createLiftTargetRow model max 0.9 "3+"
            ]
        , div [ class "week 531" ]
            [ button [ class "row header" ] [ text "5-3-1" ]
            , createLiftTargetRow model max 0.75 "5"
            , createLiftTargetRow model max 0.85 "3"
            , createLiftTargetRow model max 0.95 "1+"
            ]
        ]


createLiftTargetRow : Model -> Int -> Float -> String -> Html Msg
createLiftTargetRow model max percent count =
    let
        lift : Float
        lift =
            roundToFive (percent * toFloat max)

        plates : List Float
        plates =
            calcPlates (lift - model.bar) model.plates

        plateDisplay : String
        plateDisplay =
            String.join ", " (List.map String.fromFloat plates)
    in
    div [ class "row lift" ]
        [ span [ class "weight" ] [ text (String.fromFloat lift ++ " lbs") ]
        , span [ class "count" ] [ text ("x" ++ count) ]
        , span [ class "plates" ] [ text ("[" ++ plateDisplay ++ "]") ]
        ]


roundToFive : Float -> Float
roundToFive weight =
    toFloat (5 * round (weight / 5))


calcPlates : Float -> List Float -> List Float
calcPlates remaining plates =
    case plates of
        [] ->
            []

        largest :: rest ->
            if remaining <= 0 then
                []

            else if (2 * largest) > remaining then
                calcPlates remaining rest

            else
                largest :: calcPlates (remaining - (2 * largest)) plates
