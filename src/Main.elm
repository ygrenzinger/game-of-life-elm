module Main exposing (main)

import Array exposing (Array)
import Browser exposing (sandbox)
import GameOfLife exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import Time


type alias Model =
    { size : Int, gameOfLife : GameOfLife, running : Bool }


init : () -> ( Model, Cmd Message )
init _ =
    let
        size = 30
    in
        ( {
            size = size,
            gameOfLife = createEmptyGameOfLife size,
            running = False
          }
        , Cmd.none
        )


type Message
    = Tick Time.Posix
    | CreateGrid
    | SwitchCellState Position
    | Run
    | Pause
    | Reset


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Tick _ ->
            let
                ticker = if model.running
                    then nextGeneration model.gameOfLife
                    else model.gameOfLife
            in
            (
                { model | gameOfLife = ticker }
                , Cmd.none
            )
        CreateGrid ->
            ( model, Cmd.none )
        SwitchCellState pos ->
            ( { model | gameOfLife = switchCellStateAtPos model.gameOfLife pos }, Cmd.none )
        Run ->
            ( { model | running = True }, Cmd.none )
        Pause ->
            ( { model | running = False }, Cmd.none )
        Reset ->
            ( { model | gameOfLife = createEmptyGameOfLife model.size }, Cmd.none )

displayCell : Cell -> Html Message
displayCell (Cell position state) =
    let
        className : String
        className = "cell " ++ (if state == Alive then "alive" else "dead")
    in
        span [
            onClick (SwitchCellState position)
            , class className
            ][]

displayRow : Array Cell -> Html Message
displayRow row =
    div [
        class "row"
    ] (Array.map displayCell row |> Array.toList)


displayGameOfLife : GameOfLife -> Html Message
displayGameOfLife (GameOfLife _ grid) =  div [] (Array.map displayRow grid |> Array.toList)

view : Model -> Html Message
view model =
    div []
        [ button [ onClick Run ] [ text "Run" ]
        , button [ onClick Pause ] [ text "Pause" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , displayGameOfLife model.gameOfLife
        ]

subscriptions : Model -> Sub Message
subscriptions _ =
    Time.every 1000 Tick

main : Program () Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
