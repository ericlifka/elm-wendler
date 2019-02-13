module Workouts exposing (Workout, Workouts, workouts)


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
