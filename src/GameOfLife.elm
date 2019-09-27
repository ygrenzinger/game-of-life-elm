module GameOfLife exposing (..)

import Array exposing (..)
import Maybe exposing (andThen)

type CellState = Alive | Dead

type Cell = Cell Position CellState

type alias Row = Array Cell
type alias Grid = Array Row

type alias Position = (Int, Int)

type GameOfLife = GameOfLife Int Grid

sizeOf : GameOfLife -> Int
sizeOf (GameOfLife size _) = size

buildGridPositions : List Int -> List Int -> List Position
buildGridPositions rangeA rangeB =
    List.concatMap (\i -> (List.map (\j -> (i, j)) rangeB)) rangeA

invertCell : Cell -> Cell
invertCell (Cell position state) =
    let
        nextState = case state of
            Alive -> Dead
            Dead -> Alive
     in
        Cell position nextState

nextCellState :  CellState -> Int -> CellState
nextCellState state nbOfAliveNeighbours =
    case ( state, nbOfAliveNeighbours ) of
        ( _, 3 ) -> Alive
        ( Alive, 2 ) -> Alive
        _ -> Dead

nextCell : Int -> Cell -> Cell
nextCell nbOfAliveNeighbours (Cell pos state) = Cell pos (nextCellState state nbOfAliveNeighbours)

switchCellStateAtPos : GameOfLife -> Position -> GameOfLife
switchCellStateAtPos gameOfLife position =
    cellAt gameOfLife position
        |> Maybe.map invertCell
        |> Maybe.andThen (updateCellAt gameOfLife)
        |> Maybe.withDefault gameOfLife

createGameOfLife : CellState -> Int -> GameOfLife
createGameOfLife cellState size =
    let
        indices = List.range 0 (size - 1)
        buildCell = \rowIndex columnIndex -> Cell (rowIndex, columnIndex) cellState
        buildRow = \rowIndex -> List.map (buildCell rowIndex) indices |> fromList
        grid = List.map buildRow indices |> fromList
    in
        GameOfLife size grid

createEmptyGameOfLife : Int -> GameOfLife
createEmptyGameOfLife = createGameOfLife Dead

createFullGameOfLife : Int -> GameOfLife
createFullGameOfLife = createGameOfLife Alive

cellAt : GameOfLife -> Position -> Maybe Cell
cellAt (GameOfLife _ grid) (rowIndex, columnIndex) = get rowIndex grid |> andThen (get columnIndex)

updateCellAt : GameOfLife -> Cell -> Maybe GameOfLife
updateCellAt (GameOfLife size grid) (Cell (rowIndex, columnIndex) cellState) =
    get rowIndex grid
    |> Maybe.map (set columnIndex (Cell (rowIndex, columnIndex) cellState))
    |> Maybe.map (\r -> set rowIndex r grid)
    |> Maybe.map (GameOfLife size)


isAlive : GameOfLife -> Position -> Bool
isAlive gameOfLife position =
    case (cellAt gameOfLife position) of
      Nothing -> False
      Just (Cell _ Dead) -> False
      Just (Cell _ Alive) -> True

buildNeighboringPosition : Position -> List Position
buildNeighboringPosition (rowIndex, columnIndex) =
    let
        rowIndices = List.range (rowIndex - 1) (rowIndex + 1)
        columnIndices = List.range (columnIndex - 1) (columnIndex + 1)
    in
        buildGridPositions rowIndices columnIndices
            |> List.filter (\(i,j) -> not(i == rowIndex && j == columnIndex))

countAliveNeighbours : Position -> GameOfLife -> Int
countAliveNeighbours position gameOfLife =
  List.filter (isAlive gameOfLife) (buildNeighboringPosition position) |> List.length

nextGenerationForPosition : Position -> GameOfLife -> GameOfLife -> Maybe GameOfLife
nextGenerationForPosition position currentGen nextGen =
    let
        count = countAliveNeighbours position currentGen
    in
        cellAt currentGen position
            |> Maybe.map (nextCell count)
            |> Maybe.andThen (updateCellAt nextGen)

nextGeneration : GameOfLife -> GameOfLife
nextGeneration currentGen =
  let
    size = sizeOf currentGen
    indices = List.range 0 (size - 1)
    gameOfLife = (createEmptyGameOfLife (size))
    nextGenerationFoldOp = \pos nextGen -> Maybe.andThen (nextGenerationForPosition pos currentGen) nextGen
  in
    List.foldl nextGenerationFoldOp (Just gameOfLife) (buildGridPositions indices indices)
        |> Maybe.withDefault currentGen


