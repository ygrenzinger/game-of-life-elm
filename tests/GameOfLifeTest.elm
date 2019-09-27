module GameOfLifeTest exposing (..)

import Array
import Expect exposing (Expectation)
import Test exposing (..)

import GameOfLife exposing (..)

convertState : String -> CellState
convertState r = if r == "x" then Alive else Dead

convertCell : Int -> Int -> String -> Cell
convertCell rowIndex columnIndex state = Cell (rowIndex, columnIndex) (convertState state)

convertFrom : List (List String) -> GameOfLife
convertFrom r =
    let
        size = List.length r
        buildRow = \rowIndex row -> List.indexedMap (convertCell rowIndex) row |> Array.fromList
        grid = List.indexedMap buildRow r |> Array.fromList
    in
        GameOfLife size grid

suite : Test
suite =
    describe "Game Of Life tests"
        [ describe "Next cell state" -- Nest as many descriptions as you like.
            [ test "Any live cell with fewer than two live neighbours dies, as if by underpopulation."
                <| \_ -> List.range 0 1
                    |> List.all (\nbAliveNeighbours -> (nextCellState Alive nbAliveNeighbours) == Dead)
                    |> Expect.true "Expect cell state to be dead"
            , test "Any live cell with two or three live neighbours lives on to the next generation"
                    <| \_ -> List.range 2 3
                        |> List.all (\nbAliveNeighbours -> (nextCellState Alive nbAliveNeighbours) == Alive)
                        |> Expect.true "Expect cell state to be alive"
            , test "Any live cell with more than three live neighbours dies, as if by overpopulation"
                    <| \_ -> List.range 4 8
                        |> List.all (\nbAliveNeighbours -> (nextCellState Alive nbAliveNeighbours) == Dead)
                        |> Expect.true "Expect cell state to be dead"
            , test "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction"
                    <| \_ -> nextCellState Dead 3
                     |> Expect.equal Alive
            , test "Any dead cell with exactly not three live neighbours stays dead"
                    <| \_ -> List.range 0 8
                        |> List.filter (\x -> x /= 3)
                        |> List.all (\nbAliveNeighbours -> (nextCellState Dead nbAliveNeighbours) == Dead)
                        |> Expect.true "Expect cell state to be dead"
            ]
        , describe "Computing alive neighbours" -- Nest as many descriptions as you like.
            [ test "no alive neighbours"
                <| \_ -> createEmptyGameOfLife 3
                    |> countAliveNeighbours (1,1)
                    |> Expect.equal 0
            , test "all alive neighbours"
                <| \_ -> createFullGameOfLife 3
                    |> countAliveNeighbours (1,1)
                    |> Expect.equal 8
             ]
        , describe "convert From Test Representation"
            [ test  "should convert string to GameOfLife"
                <| \_ -> convertFrom
                    [
                        ["x", "x", "x"],
                        ["x", "x", "x"],
                        ["x", "x", "x"]
                    ]
                    |> Expect.equal (createFullGameOfLife 3)
            ]
        , describe "Validating generation with patterns"
            [ test  "should handle Still lifes pattern with block example"
                <| \_ ->
                    let
                        gameOfLife = convertFrom
                            [
                                [" ", " ", " ", " "],
                                [" ", "x", "x", " "],
                                [" ", "x", "x", " "],
                                [" ", " ", " ", " "]
                            ]
                        expected = convertFrom
                            [
                                [" ", " ", " ", " "],
                                [" ", "x", "x", " "],
                                [" ", "x", "x", " "],
                                [" ", " ", " ", " "]
                            ]
                     in
                        nextGeneration gameOfLife |> Expect.equal expected
            , test  "should handle Oscillators pattern with blinker example"
                <| \_ ->
                    let
                      gameOfLife = convertFrom
                          [
                              [" ", " ", " ", " ", " "],
                              [" ", " ", " ", " ", " "],
                              [" ", "x", "x", "x", " "],
                              [" ", " ", " ", " ", " "],
                              [" ", " ", " ", " ", " "]
                          ]
                      expected = convertFrom
                          [
                              [" ", " ", " ", " ", " "],
                              [" ", " ", "x", " ", " "],
                              [" ", " ", "x", " ", " "],
                              [" ", " ", "x", " ", " "],
                              [" ", " ", " ", " ", " "]
                          ]
                    in
                      nextGeneration gameOfLife |> Expect.equal expected
            ]
        ]