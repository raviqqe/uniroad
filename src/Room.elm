module Room exposing (Room, generate, toPositions)

import List
import Position exposing (Position)
import Random exposing (Generator)
import Set exposing (Set)


type alias Room =
    { leftTop : Position, rightBottom : Position }


generate : ( Int, Int ) -> ( Int, Int ) -> Generator Room
generate ( minX, maxX ) ( minY, maxY ) =
    Random.map
        (\( p1, p2 ) -> { leftTop = p1, rightBottom = p2 })
        (Random.andThen
            (\position ->
                Random.pair
                    (Random.constant position)
                    (Position.generate
                        ( Position.x position + minWidth, maxX )
                        ( Position.y position + minHeight, maxY )
                    )
            )
            (Position.generate ( minX, maxX - minWidth ) ( minY, maxY - minHeight ))
        )


toPositions : Room -> Set ( Int, Int )
toPositions room =
    Set.fromList
        (List.concat
            (List.map
                (\x ->
                    List.map
                        (\y -> ( x, y ))
                        (List.range (Position.y room.leftTop) (Position.y room.rightBottom))
                )
                (List.range (Position.x room.leftTop) (Position.x room.rightBottom))
            )
        )


minWidth : Int
minWidth =
    4


minHeight : Int
minHeight =
    4
