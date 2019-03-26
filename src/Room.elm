module Room exposing (Room, generate, toPositions)

import List
import Position exposing (Position)
import Random exposing (Generator)
import Set exposing (Set)


type alias Room =
    { leftTop : Position, rightBottom : Position }


generate : ( Int, Int ) -> ( Int, Int ) -> Generator Room
generate ( minX, maxX ) ( minY, maxY ) =
    Position.generate ( minX, maxX - minWidth ) ( minY, maxY - minHeight )
        |> Random.andThen
            (\position ->
                Random.pair
                    (Random.constant position)
                    (let
                        ( x, y ) =
                            position
                     in
                     Position.generate ( x + minWidth, maxX ) ( y + minHeight, maxY )
                    )
            )
        |> Random.map (\( p1, p2 ) -> { leftTop = p1, rightBottom = p2 })


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


minWidth : Int
minWidth =
    4


minHeight : Int
minHeight =
    4
