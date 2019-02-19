module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, node, span, text)
import Html.Attributes exposing (attribute, class, classList, id)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (map, map3)
import LocalStorage exposing (Event(..))
import Tuple exposing (second)
import Workouts exposing (..)


{-| This creates the most basic sort of Elm progam available in the
browser. No side effects like HTTP requests are available, just user
input and view rendering. For more options, see the elm/browser package
documentation @ <https://package.elm-lang.org/packages/elm/browser/latest/>
-}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type OpenView
    = SettingsView
    | WorkoutView


type OpenWeek
    = NoWeek
    | FiveWeek
    | ThreeWeek
    | OneWeek
    | DeloadWeek


type OpenMovement
    = NoMovement
    | BenchMovement
    | SquatMovement
    | DeadliftMovement
    | PressMovement


type alias Model =
    { bench : Int
    , squat : Int
    , deadlift : Int
    , press : Int
    , bar : Float
    , plates : List Float
    , openView : OpenView
    , openWeek : OpenWeek
    , openMovement : OpenMovement
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bench = 0
      , squat = 0
      , deadlift = 0
      , press = 0
      , bar = 45
      , plates = [ 45, 25, 10, 5, 2.5 ]
      , openView = WorkoutView
      , openWeek = NoWeek
      , openMovement = NoMovement
      }
    , Cmd.batch
        [ LocalStorage.request "bench"
        , LocalStorage.request "squat"
        , LocalStorage.request "deadlift"
        , LocalStorage.request "press"
        ]
    )



-- Update


type Msg
    = AddBench Int
    | AddSquat Int
    | AddDeadlift Int
    | AddPress Int
    | SwitchView OpenView
    | SwitchWeek OpenWeek
    | SwitchMovement OpenMovement
    | StorageEvent LocalStorage.Event


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddBench value ->
            ( model, saveLift "bench" (model.bench + value) )

        AddSquat value ->
            ( model, saveLift "squat" (model.squat + value) )

        AddDeadlift value ->
            ( model, saveLift "deadlift" (model.deadlift + value) )

        AddPress value ->
            ( model, saveLift "press" (model.press + value) )

        SwitchView newView ->
            ( { model | openView = newView }, Cmd.none )

        SwitchWeek week ->
            ( { model | openWeek = week }, Cmd.none )

        SwitchMovement movement ->
            ( { model | openMovement = movement }, Cmd.none )

        StorageEvent event ->
            handleStorageEvent model event


handleStorageEvent : Model -> LocalStorage.Event -> ( Model, Cmd Msg )
handleStorageEvent model event =
    case event of
        Updated key value ->
            storageUpdate model key value

        WriteFailure key value err ->
            storageUpdate model key value
                |> withErrorLog
                    ("unable to write to localStorage key '"
                        ++ key
                        ++ "': "
                        ++ err
                    )

        BadMessage err ->
            ( model, Cmd.none )
                |> withErrorLog ("Malformed storage event: " ++ Decode.errorToString err)


storageUpdate : Model -> String -> Maybe String -> ( Model, Cmd Msg )
storageUpdate model key value =
    Maybe.map (updateLift model key) value
        |> Maybe.withDefault (resetLift model key)


saveLift : String -> Int -> Cmd Msg
saveLift lift value =
    LocalStorage.save lift (String.fromInt value)


updateLift : Model -> String -> String -> ( Model, Cmd Msg )
updateLift model lift valStr =
    case String.toInt valStr of
        Nothing ->
            ( model, logError ("Got invalid int value: (" ++ lift ++ ", " ++ valStr ++ ")") )

        Just value ->
            case lift of
                "bench" ->
                    ( { model | bench = value }, Cmd.none )

                "squat" ->
                    ( { model | squat = value }, Cmd.none )

                "deadlift" ->
                    ( { model | deadlift = value }, Cmd.none )

                "press" ->
                    ( { model | press = value }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


resetLift : Model -> String -> ( Model, Cmd Msg )
resetLift model lift =
    case lift of
        "bench" ->
            ( { model | bench = 65 }, Cmd.none )

        "squat" ->
            ( { model | squat = 85 }, Cmd.none )

        "deadlift" ->
            ( { model | deadlift = 135 }, Cmd.none )

        "press" ->
            ( { model | press = 45 }, Cmd.none )

        _ ->
            ( model, Cmd.none )


withErrorLog : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withErrorLog err updateTuple =
    updateTuple
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, logError err ])


logError : String -> Cmd Msg
logError error =
    let
        log =
            Debug.log "ERROR" error
    in
    Cmd.none



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map StorageEvent LocalStorage.watchChanges



-- View


view : Model -> Html Msg
view model =
    div [ class "application" ]
        (case model.openView of
            SettingsView ->
                [ settingsButton (SwitchView WorkoutView)
                , backButton (SwitchView WorkoutView)
                , settingsView model
                ]

            WorkoutView ->
                [ settingsButton (SwitchView SettingsView)
                , workoutTopMenuView model
                ]
        )


settingsView : Model -> Html Msg
settingsView model =
    div [ class "settings-view" ]
        [ div [ class "title-bar" ] [ text "Settings" ]
        , liftMaxRow "Bench" model.bench AddBench
        , liftMaxRow "Squat" model.squat AddSquat
        , liftMaxRow "Deadlift" model.deadlift AddDeadlift
        , liftMaxRow "Press" model.press AddPress
        ]


workoutTopMenuView : Model -> Html Msg
workoutTopMenuView model =
    case model.openWeek of
        NoWeek ->
            div [ class "workout-view" ]
                [ div [ class "title-bar" ] [ text "Wendler" ]
                , button [ class "row button", onClick (SwitchWeek FiveWeek) ] [ text "5/5/5" ]
                , button [ class "row button", onClick (SwitchWeek ThreeWeek) ] [ text "3/3/3" ]
                , button [ class "row button", onClick (SwitchWeek OneWeek) ] [ text "5/3/1" ]
                , button [ class "row button", onClick (SwitchWeek DeloadWeek) ] [ text "Deload" ]
                ]

        FiveWeek ->
            workoutSubMenuView model workouts.five

        ThreeWeek ->
            workoutSubMenuView model workouts.three

        OneWeek ->
            workoutSubMenuView model workouts.one

        DeloadWeek ->
            workoutSubMenuView model workouts.deload


workoutSubMenuView : Model -> Workout -> Html Msg
workoutSubMenuView model workout =
    case model.openMovement of
        NoMovement ->
            div [ class "workout-view" ]
                [ div [ class "title-bar" ] [ text workout.name ]
                , backButton (SwitchWeek NoWeek)
                , button [ class "row button", onClick (SwitchMovement BenchMovement) ] [ text "Bench" ]
                , button [ class "row button", onClick (SwitchMovement SquatMovement) ] [ text "Squat" ]
                , button [ class "row button", onClick (SwitchMovement DeadliftMovement) ] [ text "Deadlift" ]
                , button [ class "row button", onClick (SwitchMovement PressMovement) ] [ text "Press" ]
                ]

        BenchMovement ->
            workoutView model "Bench" model.bench workout

        SquatMovement ->
            workoutView model "Squat" model.squat workout

        DeadliftMovement ->
            workoutView model "Deadlift" model.deadlift workout

        PressMovement ->
            workoutView model "Press" model.press workout


workoutView : Model -> String -> Int -> Workout -> Html Msg
workoutView model movement max workout =
    let
        header =
            [ div [ class "title-bar" ] [ text (movement ++ " " ++ workout.name) ]
            , backButton (SwitchMovement NoMovement)
            ]

        warmupSection =
            workoutSectionView model "Warmup" max workouts.warmup

        workoutSection =
            workoutSectionView model "Workout" max workout
    in
    div [ class "workout-view" ]
        (header
            ++ (if workout == workouts.deload then
                    [ workoutSection ]

                else
                    [ warmupSection, workoutSection ]
               )
        )


workoutSectionView : Model -> String -> Int -> Workout -> Html Msg
workoutSectionView model title max workout =
    let
        lifts : List Float
        lifts =
            applyWorkout workout max model.bar

        counts : List String
        counts =
            map second workout.movements

        platesList : List String
        platesList =
            platesDisplay model.bar model.plates lifts

        rows : List ( Float, String, String )
        rows =
            map3 triple lifts counts platesList
    in
    div [ class "section" ]
        (div [ class "row title" ] [ text title ]
            :: map createWorkoutRow rows
        )


createWorkoutRow : ( Float, String, String ) -> Html Msg
createWorkoutRow ( lift, count, plates ) =
    div [ class "row lift" ]
        [ span [ class "weight" ] [ text (String.fromFloat lift) ]
        , span [ class "label" ] [ text "lbs" ]
        , span [ class "count" ] [ text ("x" ++ count) ]
        , span [ class "plates" ] [ text ("[ " ++ plates ++ " ]") ]
        ]


platesDisplay : Float -> List Float -> List Float -> List String
platesDisplay bar platesSpec lifts =
    lifts
        |> map (\lift -> lift - bar)
        |> map (calcPlates platesSpec)
        |> map
            (\plates ->
                plates
                    |> map String.fromFloat
                    |> String.join ", "
            )


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


calcPlates : List Float -> Float -> List Float
calcPlates plates remaining =
    case plates of
        [] ->
            []

        largest :: rest ->
            if remaining <= 0 then
                []

            else if (2 * largest) > remaining then
                calcPlates rest remaining

            else
                largest :: calcPlates plates (remaining - (2 * largest))


{-| TODO: convert to editable input fields
-}
liftMaxRow : String -> Int -> (Int -> Msg) -> Html Msg
liftMaxRow lift max changeLift =
    div [ class "row" ]
        [ button [ onClick (changeLift -5) ] [ text "-5" ]
        , div [ class "label" ]
            [ text lift ]
        , div [ class "value" ]
            [ text (String.fromInt max) ]
        , button [ onClick (changeLift 5) ] [ text "+5" ]
        ]


settingsButton : Msg -> Html Msg
settingsButton openView =
    div [ class "settings-button-wrapper" ]
        [ button [ class "nav-button", onClick openView ]
            [ ionicon "settings" ]
        ]


backButton : Msg -> Html Msg
backButton openView =
    div [ class "back-button-wrapper" ]
        [ button [ class "nav-button", onClick openView ]
            [ ionicon "arrow-round-back" ]
        ]


ionicon : String -> Html Msg
ionicon icon =
    node "ion-icon" [ attribute "name" icon ] []
