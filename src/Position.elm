module Position exposing (Position, generate, init, x, y)

import Random


type alias Position =
    ( Int, Int )


init : Int -> Int -> Position
init newX newY =
    ( newX, newY )


generate : ( Int, Int ) -> ( Int, Int ) -> Random.Generator Position
generate ( minX, maxX ) ( minY, maxY ) =
    Random.map2
        (\newX newY -> ( newX, newY ))
        (Random.int minX maxX)
        (Random.int minY maxY)


x : Position -> Int
x =
    Tuple.first


y : Position -> Int
y =
    Tuple.second
