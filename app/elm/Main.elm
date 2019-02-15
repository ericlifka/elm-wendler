module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, node, span, text)
import Html.Attributes exposing (attribute, class, classList, id)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage exposing (Event(..))
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


type OpenWorkout
    = NoWorkout
    | WarmupWorkout
    | FiveWorkout
    | ThreeWorkout
    | OneWorkout


type OpenGroup
    = NoGroup
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bench = 65
      , squat = 85
      , deadlift = 135
      , press = 45
      , bar = 45
      , plates = [ 45, 25, 10, 5, 2.5 ]
      , openGroup = NoGroup
      , openWorkout = WarmupWorkout
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
    | ToggleGroup OpenGroup
    | ToggleWorkout OpenWorkout
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

        ToggleGroup group ->
            ( { model
                | openGroup =
                    if model.openGroup == group then
                        NoGroup

                    else
                        group
              }
            , Cmd.none
            )

        ToggleWorkout workout ->
            ( { model | openWorkout = workout }, Cmd.none )

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
        [ settingsButton
        , backButton
        ]


settingsButton : Html Msg
settingsButton =
    button [ class "settings-button" ]
        [ ionicon "settings" ]


backButton : Html Msg
backButton =
    button [ class "back-button" ]
        [ ionicon "arrow-round-back" ]


ionicon : String -> Html Msg
ionicon icon =
    node "ion-icon" [ attribute "name" icon ] []



-- view : Model -> Html Msg
-- view model =
--     div []
--         [ node "ion-icon" [ attribute "name" "settings" ] []
--         , div [ class "lift-maxes" ]
--             [ liftMaxRow "Bench" model.bench AddBench
--             , liftMaxRow "Squat" model.squat AddSquat
--             , liftMaxRow "Deadlift" model.deadlift AddDeadlift
--             , liftMaxRow "Press" model.press AddPress
--             ]
--         , div [ class "lift-groups" ]
--             [ liftGroup "Bench" model model.bench BenchGroup
--             , liftGroup "Squat" model model.squat SquatGroup
--             , liftGroup "Deadlift" model model.deadlift DeadliftGroup
--             , liftGroup "Press" model model.press PressGroup
--             ]
--         ]
-- liftMaxRow : String -> Int -> (Int -> Msg) -> Html Msg
-- liftMaxRow name lift addLift =
--     div [ class ("row " ++ name) ]
--         [ button [ onClick (addLift -5) ] [ text "-5" ]
--         , div [ class "label" ]
--             [ text name ]
--         , div [ class "value" ]
--             [ text (String.fromInt lift) ]
--         , button [ onClick (addLift 5) ] [ text "+5" ]
--         ]
-- liftGroup : String -> Model -> Int -> OpenGroup -> Html Msg
-- liftGroup name model lift group =
--     div
--         [ classList
--             [ ( "group", True )
--             , ( name, True )
--             , ( "visible", model.openGroup == group )
--             ]
--         ]
--         [ button
--             [ onClick (ToggleGroup group)
--             , classList
--                 [ ( "group-header header", True )
--                 , ( "active", model.openGroup == group )
--                 ]
--             ]
--             [ text name ]
--         , createLiftWorkout model lift workouts.warmup WarmupWorkout
--         , createLiftWorkout model lift workouts.five FiveWorkout
--         , createLiftWorkout model lift workouts.three ThreeWorkout
--         , createLiftWorkout model lift workouts.one OneWorkout
--         ]
-- createLiftWorkout : Model -> Int -> Workout -> OpenWorkout -> Html Msg
-- createLiftWorkout model lift workout sectionMsg =
--     let
--         buttonElement =
--             button
--                 [ onClick (ToggleWorkout sectionMsg)
--                 , classList
--                     [ ( "row header", True )
--                     , ( "active", sectionMsg == model.openWorkout )
--                     ]
--                 ]
--                 [ text workout.name ]
--         rowList =
--             List.map (createLiftTargetRow model lift) workout.movements
--     in
--     div
--         [ classList
--             [ ( "week", True )
--             , ( workout.name, True )
--             , ( "hidden", sectionMsg /= model.openWorkout )
--             ]
--         ]
--         (buttonElement :: rowList)
-- createLiftTargetRow : Model -> Int -> ( Float, String ) -> Html Msg
-- createLiftTargetRow model liftMax ( percent, count ) =
--     let
--         lift : Float
--         lift =
--             max model.bar (roundToFive (percent * toFloat liftMax))
--         plates : List Float
--         plates =
--             calcPlates (lift - model.bar) model.plates
--         plateDisplay : String
--         plateDisplay =
--             String.join ", " (List.map String.fromFloat plates)
--     in
--     div [ class "row lift" ]
--         [ span [ class "weight" ] [ text (String.fromFloat lift ++ " lbs") ]
--         , span [ class "count" ] [ text ("x" ++ count) ]
--         , span [ class "plates" ] [ text ("[" ++ plateDisplay ++ "]") ]
--         ]
-- roundToFive : Float -> Float
-- roundToFive weight =
--     toFloat (5 * floor (weight / 5))
-- calcPlates : Float -> List Float -> List Float
-- calcPlates remaining plates =
--     case plates of
--         [] ->
--             []
--         largest :: rest ->
--             if remaining <= 0 then
--                 []
--             else if (2 * largest) > remaining then
--                 calcPlates remaining rest
--             else
--                 largest :: calcPlates (remaining - (2 * largest)) plates
