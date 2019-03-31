module Division exposing (Division, bottom, init, left, right, toCorridorPositions, top)

import Position exposing (Position)
import Set exposing (Set)


type alias Division =
    { leftTop : Position, rightBottom : Position }


init : Position -> Position -> Division
init leftTop rightBottom =
    { leftTop = leftTop, rightBottom = rightBottom }


toCorridorPositions : Division -> Set Position
toCorridorPositions division =
    let
        minX =
            left division - 1

        maxX =
            right division + 1

        minY =
            top division - 1

        maxY =
            bottom division + 1
    in
    [ List.map (\x -> List.map (\y -> ( x, y )) (List.range minY maxY) ++ []) [ minX, maxX ]
    , List.map (\y -> List.map (\x -> ( x, y )) (List.range minX maxX) ++ []) [ minY, maxY ]
    ]
        |> List.concat
        |> List.concat
        |> Set.fromList


left : Division -> Int
left division =
    Position.x division.leftTop


right : Division -> Int
right division =
    Position.x division.rightBottom


top : Division -> Int
top division =
    Position.y division.leftTop


bottom : Division -> Int
bottom division =
    Position.y division.rightBottom
