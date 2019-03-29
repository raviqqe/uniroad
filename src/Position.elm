module Position exposing (Position, generate, init, move, x, y)

import Direction exposing (..)
import Random exposing (Generator)


type alias Position =
    ( Int, Int )


init : Int -> Int -> Position
init newX newY =
    ( newX, newY )


generate : ( Int, Int ) -> ( Int, Int ) -> Generator Position
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


move : Position -> Direction -> Position
move ( oldX, oldY ) direction =
    case direction of
        Left ->
            ( oldX - 1, oldY )

        Right ->
            ( oldX + 1, oldY )

        Up ->
            ( oldX, oldY - 1 )

        Down ->
            ( oldX, oldY + 1 )

        LeftUp ->
            ( oldX - 1, oldY - 1 )

        LeftDown ->
            ( oldX - 1, oldY + 1 )

        RightUp ->
            ( oldX + 1, oldY - 1 )

        RightDown ->
            ( oldX + 1, oldY + 1 )
