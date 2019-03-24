module Floor exposing (Floor, generate, height, inside, width)

import Position exposing (Position)
import Random
import Room
import Set exposing (Set)


type alias Floor =
    { validPositions : Set Position
    }


generate : Random.Generator Floor
generate =
    Random.map
        (\room -> { validPositions = Room.toPositions room })
        (Room.generate ( 2, 31 ) ( 2, 31 ))


inside : Floor -> Position -> Bool
inside floor position =
    Set.member position floor.validPositions


width : Int
width =
    32


height : Int
height =
    32
