module Workouts exposing (Workout, Workouts, applyWorkout, workouts)

import List exposing (map)
import Tuple exposing (first)


type alias Workout =
    { name : String
    , movements : List ( Float, String )
    }


type alias Workouts =
    { warmup : Workout
    , five : Workout
    , three : Workout
    , one : Workout
    , deload : Workout
    }


workouts : Workouts
workouts =
    { warmup =
        { name = "Warmup"
        , movements =
            [ ( 0.4, "5" )
            , ( 0.5, "5" )
            , ( 0.6, "3" )
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
    , deload =
        { name = "Deload"
        , movements =
            [ ( 0.4, "5" )
            , ( 0.5, "5" )
            , ( 0.6, "5" )
            ]
        }
    }


applyWorkout : Workout -> Int -> Float -> List Float
applyWorkout workout max min =
    workout.movements
        |> map first
        |> List.map ((*) (toFloat max))
        |> map (Basics.max min)
        |> map roundToFive


roundToFive : Float -> Float
roundToFive weight =
    toFloat (5 * floor (weight / 5))
