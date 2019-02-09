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
        { init = initialModel
        , update = update
        , view = view
        }



-- Model


type alias Lift =
    { name : String
    , max : Int
    , visible : Bool
    }


type alias Model =
    { bench : Lift
    , squat : Lift
    , deadlift : Lift
    , press : Lift
    , bar : Float
    , plates : List Float
    , warmupVisible : Bool
    , fiveWeekVisible : Bool
    , threeWeekVisible : Bool
    , oneWeekVisible : Bool
    }


initialModel : Model
initialModel =
    { bench =
        { name = "Bench"
        , max = 65
        , visible = False
        }
    , squat =
        { name = "Squat"
        , max = 85
        , visible = False
        }
    , deadlift =
        { name = "Deadlift"
        , max = 135
        , visible = False
        }
    , press =
        { name = "Press"
        , max = 45
        , visible = False
        }
    , bar = 45
    , plates = [ 45, 35, 25, 10, 5, 2.5 ]
    , warmupVisible = True
    , fiveWeekVisible = False
    , threeWeekVisible = False
    , oneWeekVisible = False
    }


type alias Workout =
    { name : String
    , movements : List ( Float, String )
    }


type alias Workouts =
    { warmup : Workout
    , five : Workout
    , three : Workout
    , one : Workout
    }


workouts : Workouts
workouts =
    { warmup =
        { name = "Warmup/Deload"
        , movements =
            [ ( 0.4, "5" )
            , ( 0.5, "5" )
            , ( 0.6, "5" )
            ]
        }
    , five =
        { name = "5-5-5"
        , movements =
            [ ( 0.65, "5" )
            , ( 0.75, "5" )
            , ( 0.85, "5+" )
            ]
        }
    , three =
        { name = "3-3-3"
        , movements =
            [ ( 0.7, "3" )
            , ( 0.8, "3" )
            , ( 0.9, "3+" )
            ]
        }
    , one =
        { name = "5-3-1"
        , movements =
            [ ( 0.75, "5" )
            , ( 0.85, "3" )
            , ( 0.95, "1+" )
            ]
        }
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
    | ToggleWarmup
    | ToggleFiveWeek
    | ToggleThreeWeek
    | ToggleOneWeek


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddBench value ->
            { model | bench = updateLiftMax model.bench value }

        AddSquat value ->
            { model | squat = updateLiftMax model.squat value }

        AddDeadlift value ->
            { model | deadlift = updateLiftMax model.deadlift value }

        AddPress value ->
            { model | press = updateLiftMax model.press value }

        ToggleBench ->
            { model
                | bench = toggleSection model.bench
                , squat = hideSection model.squat
                , deadlift = hideSection model.deadlift
                , press = hideSection model.press
            }

        ToggleSquat ->
            { model
                | bench = hideSection model.bench
                , squat = toggleSection model.squat
                , deadlift = hideSection model.deadlift
                , press = hideSection model.press
            }

        ToggleDeadlift ->
            { model
                | bench = hideSection model.bench
                , squat = hideSection model.squat
                , deadlift = toggleSection model.deadlift
                , press = hideSection model.press
            }

        TogglePress ->
            { model
                | bench = hideSection model.bench
                , squat = hideSection model.squat
                , deadlift = hideSection model.deadlift
                , press = toggleSection model.press
            }

        ToggleWarmup ->
            { model
                | warmupVisible = not model.warmupVisible
                , fiveWeekVisible = False
                , threeWeekVisible = False
                , oneWeekVisible = False
            }

        ToggleFiveWeek ->
            { model
                | warmupVisible = False
                , fiveWeekVisible = not model.fiveWeekVisible
                , threeWeekVisible = False
                , oneWeekVisible = False
            }

        ToggleThreeWeek ->
            { model
                | warmupVisible = False
                , fiveWeekVisible = False
                , threeWeekVisible = not model.threeWeekVisible
                , oneWeekVisible = False
            }

        ToggleOneWeek ->
            { model
                | warmupVisible = False
                , fiveWeekVisible = False
                , threeWeekVisible = False
                , oneWeekVisible = not model.oneWeekVisible
            }



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "lift-maxes" ]
            [ liftMaxRow model.bench AddBench
            , liftMaxRow model.squat AddSquat
            , liftMaxRow model.deadlift AddDeadlift
            , liftMaxRow model.press AddPress
            ]
        , div [ class "lift-groups" ]
            [ liftGroup model model.bench ToggleBench
            , liftGroup model model.squat ToggleSquat
            , liftGroup model model.deadlift ToggleDeadlift
            , liftGroup model model.press TogglePress
            ]
        ]


liftMaxRow : Lift -> (Int -> Msg) -> Html Msg
liftMaxRow lift addLift =
    div [ class ("row " ++ lift.name) ]
        [ div [ class "label" ]
            [ text lift.name ]
        , div [ class "value" ]
            [ text (String.fromInt lift.max) ]
        , button [ onClick (addLift 5) ] [ text "+5" ]
        , button [ onClick (addLift -5) ] [ text "-5" ]
        ]


liftGroup : Model -> Lift -> Msg -> Html Msg
liftGroup model lift toggleVisible =
    div
        [ classList
            [ ( "group", True )
            , ( lift.name, True )
            , ( "visible", lift.visible )
            ]
        ]
        [ button
            [ onClick toggleVisible
            , class "group-header header"
            ]
            [ text lift.name ]
        , createLiftWorkout model lift workouts.warmup model.warmupVisible ToggleWarmup
        , createLiftWorkout model lift workouts.five model.fiveWeekVisible ToggleFiveWeek
        , createLiftWorkout model lift workouts.three model.threeWeekVisible ToggleThreeWeek
        , createLiftWorkout model lift workouts.one model.oneWeekVisible ToggleOneWeek
        ]


createLiftWorkout : Model -> Lift -> Workout -> Bool -> Msg -> Html Msg
createLiftWorkout model lift workout visible toggleMsg =
    let
        buttonElement =
            button
                [ onClick toggleMsg, class "row header" ]
                [ text workout.name ]

        rowList =
            List.map (createLiftTargetRow model lift.max) workout.movements
    in
    div
        [ classList
            [ ( "week", True )
            , ( workout.name, True )
            , ( "hidden", not visible )
            ]
        ]
        (buttonElement :: rowList)


createLiftTargetRow : Model -> Int -> ( Float, String ) -> Html Msg
createLiftTargetRow model liftMax spec =
    let
        ( percent, count ) =
            spec

        lift : Float
        lift =
            max model.bar (roundToFive (percent * toFloat liftMax))

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



-- Helpers


updateLiftMax : Lift -> Int -> Lift
updateLiftMax lift value =
    { lift | max = lift.max + value }


hideSection : Lift -> Lift
hideSection section =
    { section | visible = False }


toggleSection : Lift -> Lift
toggleSection section =
    { section | visible = not section.visible }


roundToFive : Float -> Float
roundToFive weight =
    toFloat (5 * floor (weight / 5))


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
