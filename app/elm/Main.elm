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


type OpenWorkout
    = WarmupWorkout
    | FiveWorkout
    | ThreeWorkout
    | OneWorkout


type OpenGroup
    = None
    | BenchGroup
    | SquatGroup
    | DeadliftGroup
    | PressGroup


type alias Model =
    { bench : Int
    , squat : Int
    , deadlift : Int
    , press : Int
    , bar : Float
    , plates : List Float
    , openGroup : OpenGroup
    , openWorkout : OpenWorkout
    }


initialModel : Model
initialModel =
    { bench = 65
    , squat = 85
    , deadlift = 135
    , press = 45
    , bar = 45
    , plates = [ 45, 35, 25, 10, 5, 2.5 ]
    , openGroup = None
    , openWorkout = WarmupWorkout
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
    | ToggleGroup OpenGroup
    | ToggleWorkout OpenWorkout


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

        ToggleGroup group ->
            { model
                | openGroup =
                    if model.openGroup == group then
                        None

                    else
                        group
            }

        ToggleWorkout workout ->
            { model | openWorkout = workout }



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
            [ liftGroup "Bench" model model.bench ToggleGroup BenchGroup
            , liftGroup "Squat" model model.squat ToggleGroup SquatGroup
            , liftGroup "Deadlift" model model.deadlift ToggleGroup DeadliftGroup
            , liftGroup "Press" model model.press ToggleGroup PressGroup
            ]
        ]


liftMaxRow : String -> Int -> (Int -> Msg) -> Html Msg
liftMaxRow name lift addLift =
    div [ class ("row " ++ name) ]
        [ button [ onClick (addLift -5) ] [ text "-5" ]
        , div [ class "label" ]
            [ text name ]
        , div [ class "value" ]
            [ text (String.fromInt lift) ]
        , button [ onClick (addLift 5) ] [ text "+5" ]
        ]


liftGroup : String -> Model -> Int -> (OpenGroup -> Msg) -> OpenGroup -> Html Msg
liftGroup name model lift toggleGroup group =
    div
        [ classList
            [ ( "group", True )
            , ( name, True )
            , ( "visible", model.openGroup == group )
            ]
        ]
        [ button
            [ onClick (toggleGroup group), class "group-header header" ]
            [ text name ]
        , createLiftWorkout model lift workouts.warmup ToggleWorkout WarmupWorkout
        , createLiftWorkout model lift workouts.five ToggleWorkout FiveWorkout
        , createLiftWorkout model lift workouts.three ToggleWorkout ThreeWorkout
        , createLiftWorkout model lift workouts.one ToggleWorkout OneWorkout
        ]


createLiftWorkout : Model -> Int -> Workout -> (OpenWorkout -> Msg) -> OpenWorkout -> Html Msg
createLiftWorkout model lift workout toggleVisible sectionMsg =
    let
        buttonElement =
            button
                [ onClick (toggleVisible sectionMsg), class "row header" ]
                [ text workout.name ]

        rowList =
            List.map (createLiftTargetRow model lift) workout.movements
    in
    div
        [ classList
            [ ( "week", True )
            , ( workout.name, True )
            , ( "hidden", sectionMsg /= model.openWorkout )
            ]
        ]
        (buttonElement :: rowList)


createLiftTargetRow : Model -> Int -> ( Float, String ) -> Html Msg
createLiftTargetRow model liftMax ( percent, count ) =
    let
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
