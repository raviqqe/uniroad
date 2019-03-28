module Room exposing (Room, generate, toPositions)

import List
import Position exposing (Position)
import Random exposing (Generator)
import Set exposing (Set)


type alias Room =
    { leftTop : Position, rightBottom : Position }


init : Position -> Position -> Room
init leftTop rightBottom =
    { leftTop = leftTop, rightBottom = rightBottom }


generate : Position -> Position -> Maybe (Generator Room)
generate ( minX, minY ) ( maxX, maxY ) =
    if maxX - minX < size + 1 || maxY - minY < size + 1 then
        Nothing

    else
        Position.generate ( minX + 1, maxX - 1 - size ) ( minY + 1, maxY - 1 - size )
            |> Random.andThen
                (\leftTop ->
                    let
                        ( x, y ) =
                            leftTop
                    in
                    Position.generate ( x + size, maxX - 1 ) ( y + size, maxY - 1 )
                        |> Random.map (\rightBottom -> init leftTop rightBottom)
                )
            |> Just


toPositions : Room -> Set ( Int, Int )
toPositions room =
    List.range (Position.x room.leftTop) (Position.x room.rightBottom)
        |> List.map
            (\x ->
                List.range (Position.y room.leftTop) (Position.y room.rightBottom)
                    |> List.map (\y -> ( x, y ))
            )
        |> List.concat
        |> Set.fromList


size : Int
size =
    4
