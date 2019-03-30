module Division exposing (Division, init, toSeparatorPositions)

import Position exposing (Position)
import Set exposing (Set)


type alias Division =
    { leftTop : Position, rightBottom : Position }


init : Position -> Position -> Division
init leftTop rightBottom =
    { leftTop = leftTop, rightBottom = rightBottom }


toSeparatorPositions : Division -> Set Position
toSeparatorPositions division =
    let
        ( left, top ) =
            division.leftTop

        ( right, bottom ) =
            division.rightBottom

        minX =
            left - 1

        maxX =
            right + 1

        minY =
            top - 1

        maxY =
            bottom + 1
    in
    [ List.map (\x -> List.map (\y -> ( x, y )) (List.range minY maxY) ++ []) [ minX, maxX ]
    , List.map (\y -> List.map (\x -> ( x, y )) (List.range minX maxX) ++ []) [ minY, maxY ]
    ]
        |> List.concat
        |> List.concat
        |> Set.fromList
